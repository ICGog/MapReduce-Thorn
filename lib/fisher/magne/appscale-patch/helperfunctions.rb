#!/usr/bin/ruby -w

require 'base64'
require 'openssl'
require 'socket'
require 'timeout'
require 'fileutils'

$:.unshift File.join(File.dirname(__FILE__))
require 'djinn'

MAX_VM_CREATION_TIME = 3600 # vm size ~6gb takes about ~60min to come up
SLEEP_TIME = 120
IP_REGEX = /\d+\.\d+\.\d+\.\d+/
FQDN_REGEX = /[\w\d\.\-]+/
IP_OR_FQDN = /#{IP_REGEX}|#{FQDN_REGEX}/

DELTA_REGEX = /([1-9][0-9]*)([DdHhMm]|[sS]?)/
DEFAULT_SKIP_FILES_REGEX = /^(.*\/)?((app\.yaml)|(app\.yml)|(index\.yaml)|(index\.yml)|(\#.*\#)|(.*~)|(.*\.py[co])|(.*\/RCS\/.*)|(\..*)|)$/

TIME_IN_SECONDS = { "d" => 86400, "h" => 3600, "m" => 60, "s" => 1 }

module HelperFunctions
  def self.get_network_info(ports)
    threads = []
    lo = 0
    eth0 = 0
    syntax = "tcp and (port " + ports.join(" or port ") + ")"

    ["lo", "eth0"].each { |iface|
      threads << Thread.new(iface) { |myface|
        myfile = "/root/appscale/.appscale/tcpdump-#{myface}"
        Kernel.system("tcpdump -n -q -t -l -i #{myface} '#{syntax}' > #{myfile} 2>/dev/null &")
        sleep(2)
        count = `wc -l #{myfile}`.split[0]
        eval("#{myface} = #{count}")
      }
    }

    threads.each { |thr| thr.join }
    `pkill tcpdump`
    return lo, eth0
  end

  def self.open_ports_in_cloud(ports, infrastructure)
    if ports.class == Integer
      ports = [ports]
    end
    
    ports.each { |port|
      `#{infrastructure}-authorize appscale -p #{port}`
    }
  end

  def self.sleep_until_port_is_open(ip, port, use_ssl=false)
    loop {
      return if HelperFunctions.is_port_open?(ip, port, use_ssl)
      sleep(1)
      Djinn.log_debug("Waiting on #{ip}:#{port} to be open (currently closed).")
    }
  end

  def self.sleep_until_port_is_closed(ip, port, use_ssl=false)
    loop {
      return unless HelperFunctions.is_port_open?(ip, port, use_ssl)
      sleep(1)
      Djinn.log_debug("Waiting on #{ip}:#{port} to be closed (currently open).")
    }
  end
    
  def self.is_port_open?(ip, port, use_ssl=false)
    begin
      Timeout::timeout(1) do
        begin
          sock = TCPSocket.new(ip, port)
          if use_ssl
            ssl_context = OpenSSL::SSL::SSLContext.new() 
            unless ssl_context.verify_mode 
              ssl_context.verify_mode = OpenSSL::SSL::VERIFY_NONE 
            end 
            sslsocket = OpenSSL::SSL::SSLSocket.new(sock, ssl_context) 
            sslsocket.sync_close = true 
            sslsocket.connect          
          end
          sock.close
          return true
        rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH, Errno::ECONNRESET
          return false
        end
      end
    rescue Timeout::Error
    end
  
    return false
  end

  def self.run_remote_command(ip, command, public_key_loc, want_output)
    public_key_loc = File.expand_path(public_key_loc)
    
    remote_cmd = "ssh -i #{public_key_loc} -o StrictHostkeyChecking=no root@#{ip} '#{command} "
    
    if want_output
      remote_cmd << "2>&1 > /tmp/#{ip}.log &' &"
    else
      remote_cmd << "> /dev/null &' &"
    end

    Djinn.log_debug("Running [#{remote_cmd}]")
    Kernel.system remote_cmd
    return remote_cmd
  end

  def self.scp_file(local_file_loc, remote_file_loc, target_ip, private_key_loc="~/.ssh/id_dsa")
    private_key_loc = File.expand_path(private_key_loc)
    `chmod 0600 #{private_key_loc}`
    local_file_loc = File.expand_path(local_file_loc)
    `rm -f ~/appscale/.appscale/retval`
    cmd = "scp -i #{private_key_loc} -o StrictHostkeyChecking=no 2>&1 #{local_file_loc} root@#{target_ip}:#{remote_file_loc}; echo $? >> ~/appscale/.appscale/retval"
    Djinn.log_debug(cmd)
    scp_result = `#{cmd}`

    retval_loc = File.expand_path("~/appscale/.appscale/retval")
    loop {
      break if File.exists?(retval_loc)
      sleep(5)
    }

    retval = (File.open(retval_loc) { |f| f.read }).chomp
    abort("\n\n[#{cmd}] returned #{retval} instead of 0 as expected. Is your environment set up properly?") if retval != "0"

    Djinn.log_debug(scp_result)
  end

  def self.get_appscale_id
    image_info = `ec2-describe-images`
    
    abort("ec2 tools can't find appscale image") unless image_info.include?("appscale")
    image_id = image_info.scan(/([a|e]mi-[0-9a-zA-Z]+)\sappscale/).flatten.to_s
    
    return image_id
  end

  def self.get_cert(filename)
    return nil unless File.exists?(filename)
    OpenSSL::X509::Certificate.new(File.open(filename) { |f|
      f.read
    })
  end
  
  def self.get_key(filename)
    return nil unless File.exists?(filename)
    OpenSSL::PKey::RSA.new(File.open(filename) { |f|
      f.read
    })
  end
  
  def self.get_secret(filename="~/appscale/.appscale/secret.key")
    filename = File.expand_path(filename)
    return nil unless File.exists?(filename)
    secret = (File.open(filename) { |f| f.read }).chomp
  end
  
  def self.setup_app(app_name, encoded_app_tar)
    meta_dir = "/var/apps/#{app_name}"
    tar_dir = "#{meta_dir}/app/"
    tar_path = "#{tar_dir}#{app_name}.tar.gz"
    Kernel.system "rm -rf #{tar_dir}"
    Kernel.system "mkdir -p #{tar_dir}"
    Kernel.system "mkdir -p #{meta_dir}/log"
    Kernel.system "touch #{meta_dir}/log/server.log"
    tar_file = File.open(tar_path, "w")
    decoded_tar = Base64.decode64(encoded_app_tar)
    tar_file.write(decoded_tar)
    tar_file.close
    Kernel.system "tar --file #{tar_path} --force-local -C #{tar_dir} -zx"  
  end

  def self.run_app(app_name, port, db_location, public_ip, app_version, app_language)
    secret = HelperFunctions.get_secret    

    if app_language == "python"
      Djinn.log_debug("saw a python app coming through")
      start_app = "python2.5 /root/appscale/AppServer/dev_appserver.py -p #{port} --cookie_secret #{secret} --login_server #{public_ip} --admin_console_server '' --datastore_path #{db_location} --history_path /var/apps/#{app_name}/data/app.datastore.history /var/apps/#{app_name}/app -a #{public_ip} --appscale_version #{app_version} >> /var/apps/#{app_name}/log/server.log 2>&1 &"
      Djinn.log_debug(start_app)
      Kernel.system start_app
    elsif app_language == "java"
      Djinn.log_debug("saw a java app coming through")
      `cp -r /root/appscale/AppServer_Java/lib/* /var/apps/#{app_name}/app/war/WEB-INF/lib/`
      start_app = "cd /root/appscale/AppServer_Java && ./genKeystore.sh && ./appengine-java-sdk-1.2.6_repacked/bin/dev_appserver.sh --port=#{port} --address=#{public_ip} --datastore_path=#{db_location} --cookie_secret=#{secret} --login_server=#{public_ip} --appscale_version=#{app_version} /var/apps/#{app_name}/app/war/ >> /var/apps/#{app_name}/log/server.log 2>&1 &"
      Djinn.log_debug(start_app)
      Kernel.system start_app  	
    elsif app_language == "thorn"
      Djinn.log_debug("saw a thorn app coming through")
      # assuming that Thorn at some time will resolve port conflicts itself.
      thornport = port - 1000
      controller_ip = `cat /root/appscale/.appscale/masters`.delete("\n")
      thorn_datastore = "thorn://#{controller_ip}:11200"
      thorn_memcache = "thorn://#{controller_ip}:11210"
      # var/apps/#{app_name}/app/dev_appserver.sh
      start_app = "/root/appscale/AppServer_Thorn/dev_appserver.sh /var/apps/#{app_name}/app -t #{thornport} -s #{port} -m #{thorn_memcache} -d #{thorn_datastore} >> /var/apps/#{app_name}/log/server.log 2>&1 &"
      Djinn.log_debug(start_app)
      Kernel.system start_app
    else
      Djinn.log_debug("Currently we only support python and java applications, not #{app_language}.")
    end

    HelperFunctions.sleep_until_port_is_open(HelperFunctions.local_ip, port)
    
    pid = `ps ax | grep #{port} | grep -v grep | awk '{ print $1 } '`
    Djinn.log_debug("Started app #{app_name} with pid #{pid}")
    
    return pid
  end

  # Code for local_ip taken from 
  # http://coderrr.wordpress.com/2008/05/28/get-your-local-ip-address/
  def self.local_ip
    UDPSocket.open {|s| s.connect("64.233.187.99", 1); s.addr.last }
  end

  def self.convert_fqdn_to_ip(host)
    return host if host =~ /#{IP_REGEX}/
  
    nslookup = `nslookup #{host}`
    ip = nslookup.scan(/#{host}\nAddress:\s+(#{IP_OR_FQDN})/).flatten.to_s
    abort("Couldn't convert #{host} to an IP address. Result of nslookup was \n#{nslookup}") if ip.nil? or ip == ""
    return ip
  end
  
  def self.get_ips(ips)
    abort("ips not even length array") if ips.length % 2 != 0
    reported_public = []
    reported_private = []
    ips.each_index { |index|
      if index % 2 == 0
        reported_public << ips[index]
      else
        reported_private << ips[index]
      end
    }
    
    Djinn.log_debug("Reported Public IPs: [#{reported_public.join(', ')}]")
    Djinn.log_debug("Reported Private IPs: [#{reported_private.join(', ')}]")

    actual_public = []
    actual_private = []
    
    reported_public.each_index { |index|
      pub = reported_public[index]
      pri = reported_private[index]
      if pub != "0.0.0.0" and pri != "0.0.0.0"
        actual_public << pub
        actual_private << pri
      end
    }
        
    #actual_public.each_index { |index|
    #  actual_public[index] = HelperFunctions.convert_fqdn_to_ip(actual_public[index])
    #}

    actual_private.each_index { |index|
      actual_private[index] = HelperFunctions.convert_fqdn_to_ip(actual_private[index])
    }
    
    return actual_public, actual_private
  end

  def self.get_public_ips(ips)
    abort("ips not even length array") if ips.length % 2 != 0
    reported_public = []
    reported_private = []
    ips.each_index { |index|
      if index % 2 == 0
        reported_public << ips[index]
      else
        reported_private << ips[index]
      end
    }
    
    Djinn.log_debug("Reported Public IPs: [#{reported_public.join(', ')}]")
    Djinn.log_debug("Reported Private IPs: [#{reported_private.join(', ')}]")
    
    public_ips = []
    reported_public.each_index { |index|
      if reported_public[index] != "0.0.0.0"
        public_ips << reported_public[index]
      elsif reported_private[index] != "0.0.0.0"
        public_ips << reported_private[index]
      end
    }
    
    return public_ips.flatten
  end
  
  def self.spawn_vms(num_of_vms_to_spawn, job, image_id, instance_type, keyname, infrastructure)
    return [] if num_of_vms_to_spawn < 1
    instance_ids_up = []
    public_up_already = []
    private_up_already = []
    Djinn.log_debug("[#{num_of_vms_to_spawn}] [#{job}] [#{image_id}]  [#{instance_type}] [#{keyname}] [#{infrastructure}]")
    Djinn.log_debug("EC2_URL = [#{`echo $EC2_URL`}]")
    loop { # need to make sure ec2 doesn't return an error message here
      describe_instances = `ec2-describe-instances 2>&1`
      Djinn.log_debug(describe_instances)
      all_ip_addrs = describe_instances.scan(/\s+(#{IP_OR_FQDN})\s+(#{IP_OR_FQDN})\s+running\s+#{keyname}\s/).flatten
      instance_ids_up = describe_instances.scan(/INSTANCE\s+(i-\w+)/).flatten
      public_up_already, private_up_already = HelperFunctions.get_ips(all_ip_addrs)
      vms_up_already = describe_instances.scan(/(#{IP_OR_FQDN})\s+running\s+#{keyname}\s+/).length
      break if vms_up_already > 0
    }
  
    command_to_run = "ec2-run-instances #{image_id} -k #{keyname} -n #{num_of_vms_to_spawn} --instance-type #{instance_type} --group appscale"

    loop {
      Djinn.log_debug(command_to_run)
      run_instances = `#{command_to_run} 2>&1`
      Djinn.log_debug("run_instances: [#{run_instances}]")
      if run_instances =~ /Please try again later./
        Djinn.log_debug("Error with run_instances: #{run_instances}. Will try again in a moment.")
      elsif run_instances =~ /try --addressing private/
        Djinn.log_debug("Need to retry with addressing private. Will try again in a moment.")
        command_to_run << " --addressing private"
      elsif run_instances =~ /PROBLEM/
        Djinn.log_debug("Error: #{run_instances}")
        abort("Saw the following error message from EC2 tools. Please resolve the issue and try again:\n#{run_instances}")
      else
        Djinn.log_debug("Run instances message sent successfully. Waiting for the image to start up.")
        break
      end
      Djinn.log_debug("sleepy time")
      sleep(5)
    }
    
    instance_ids = []
    public_ips = []
    private_ips = []
     
    end_time = Time.now + MAX_VM_CREATION_TIME
    while (now = Time.now) < end_time
      describe_instances = `ec2-describe-instances`
      Djinn.log_debug("[#{Time.now}] #{end_time - now} seconds left...")
      Djinn.log_debug(describe_instances)
      
      if describe_instances =~ /terminated\s+#{keyname}\s+/
        terminated_message = "An instance was unexpectedly terminated. Please contact your cloud administrator to determine why and try again. \n#{describe_instances}"
        Djinn.log_debug(terminated_message)
        abort(terminated_message)
      end
      
      # changed regexes so ensure we are only checking for instances created
      # for appscale only (don't worry about other instances created)
      
      all_ip_addrs = describe_instances.scan(/\s+(#{IP_OR_FQDN})\s+(#{IP_OR_FQDN})\s+running\s+#{keyname}\s+/).flatten
      public_ips, private_ips = HelperFunctions.get_ips(all_ip_addrs)
      public_ips = public_ips - public_up_already
      private_ips = private_ips - private_up_already
      instance_ids = describe_instances.scan(/INSTANCE\s+(i-\w+)\s+[\w\-\s\.]+#{keyname}/).flatten - instance_ids_up
      break if public_ips.length == num_of_vms_to_spawn
      sleep(SLEEP_TIME)
    end
    
    abort("No public IPs were able to be procured within the time limit.") if public_ips.length == 0
    
    if public_ips.length != num_of_vms_to_spawn
      potential_dead_ips = HelperFunctions.get_ips(all_ip_addrs) - public_up_already
      potential_dead_ips.each_index { |index|
        if potential_dead_ips[index] == "0.0.0.0"
          instance_to_term = instance_ids[index]
          Djinn.log_debug("Instance #{instance_to_term} failed to get a public IP address and is being terminated.")
          `ec2-terminate-instances #{instance_to_term}`
        end
      }
    end         
    
    # ip:job:instance-id
    instances_created = []
    public_ips.each_index { |index|
      instances_created << "#{public_ips[index]}:#{private_ips[index]}:#{job}:#{instance_ids[index]}\n"
    }
    
    return instances_created    
  end
    
  def self.terminate_vms(djinn_locations)
    instances = []
    djinn_locations.each { |djinn|
      instance_id = djinn.instance_id
      instances << instance_id
    }
    
    Djinn.log_debug(`ec2-terminate-instances #{instances.join(' ')}`)
  end
  
  def self.terminate_all_vms(keyname)
    desc_instances = `ec2-describe-instances`
    instances = desc_instances.scan(/INSTANCE\s+(i-\w+)\s+[\w\-\s\.]+#{keyname}/).flatten
    Djinn.log_debug(`ec2-terminate-instances #{instances.join(' ')}`)
  end
    
  def self.get_usage
    top_results = `top -n1 -d0 -b`
    usage = {}
    usage['cpu'] = nil
    usage['mem'] = nil
    
    if top_results =~ /Cpu\(s\):\s+([\d|\.]+)%us,\s+([\d|\.]+)%sy/
      user_cpu = Float($1)
      sys_cpu = Float($2)
      usage['cpu'] = user_cpu + sys_cpu
    end

    if top_results =~ /Mem:\s+(\d+)k total,\s+(\d+)k used/
      total_memory = Float($1)
      used_memory = Float($2)
      usage['mem'] = used_memory / total_memory * 100
    end
    
    usage    
  end

  def self.generate_location_config handler
    return "" if !handler.key?("static_dir") && !handler.key?("static_files")

    result = "\n    location #{handler['url']} {"
    result << "\n\t" << "root $cache_dir;"
    result << "\n\t" << "expires #{handler['expiration']};" if handler['expiration']

    # TODO: return a 404 page if rewritten path doesn not exist
    if handler.key?("static_dir")
      result << "\n\t" << "rewrite #{handler['url']}/(.*) /#{handler['static_dir']}/$1 break;"
    elsif handler.key?("static_files")
      result << "\n\t" << "rewrite #{handler['url']} /#{handler['static_files']} break;"
    end
    
    result << "\n" << "    }" << "\n"

    result
  end

  def self.write_nginx_config(app_name, app_number, num_of_appengines, start_port, my_public_ip, static_handlers)

    upstream_servers = ""
    num_of_appengines.times do |index|
      upstream_servers << "server #{my_public_ip}:#{start_port + app_number * num_of_appengines + index};\n    "
    end
    
    static_locations = static_handlers.map { |handler| generate_location_config(handler) }.join

    config = <<CONFIG
upstream gae_#{app_name} {
    #{upstream_servers}
}

server {
    listen #{8080 + app_number};
    server_name #{my_public_ip};
    root /var/apps/#{app_name}/app;
    access_log  /var/log/nginx/#{app_name}.access.log;
    error_log  /var/log/nginx/#{app_name}.error.log;
    rewrite_log off;

    set $cache_dir /var/apps/#{app_name}/cache;

    #{static_locations}

    location / {
      proxy_set_header  X-Real-IP  $remote_addr;
      proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Host $http_host;
      proxy_redirect false;
      proxy_pass http://gae_#{app_name};
    }
}
CONFIG

    nginx_file_loc = "/etc/nginx/sites-enabled/#{app_name}.conf"
    File.open(nginx_file_loc, "w+") { |dest_file| dest_file.write(config) }
  end

  def self.get_app_path app_name
    "/var/apps/#{app_name}/"
  end

  def self.get_cache_path app_name
    File.join(get_app_path(app_name),"cache")
  end

  # The directory where the applications tarball will be extracted to
  def self.get_untar_dir app_name
    File.join(get_app_path(app_name),"app")
  end

  # We have the files full path (e.g. ./data/myappname/static/file.txt) but we want is
  # the files path relative to the apps directory (e.g. /static/file.txt).
  # This is the hacky way of getting that.
  def self.get_relative_filename filename, app_name
    filename[get_untar_dir(app_name).length..filename.length]
  end

  def self.parse_static_data app_name
    untar_dir = get_untar_dir(app_name)

    begin
      tree = YAML.load_file(File.join(untar_dir,"app.yaml"))
    rescue Errno::ENOENT => e
      Djinn.log_debug("Failed to load YAML file to parse static data")
      return []
    end

    handlers = tree["handlers"]
    default_expiration = expires_duration(tree["default_expiration"])
    
    # Create the destination cache directory
    cache_path = get_cache_path(app_name)
    FileUtils.mkdir_p cache_path

    skip_files_regex = DEFAULT_SKIP_FILES_REGEX
    if tree["skip_files"]
      # An alternate regex has been provided for the files which should be skipped
      input_regex = tree["skip_files"]
      input_regex = input_regex.join("|") if input_regex.kind_of?(Array)

      # Remove any superfluous spaces since they will break the regex
      input_regex.gsub!(/ /,"")
      skip_files_regex = Regexp.new(input_regex)
    end

    handlers.map! do |handler|
      next if !handler.key?("static_dir") && !handler.key?("static_files")
      
      # TODO: Get the mime-type setting from app.yaml and add it to the nginx config

      if handler["static_dir"]
        cache_static_dir_path = File.join(cache_path,handler["static_dir"])
        FileUtils.mkdir_p cache_static_dir_path

        filenames = Dir.glob(File.join(untar_dir, handler["static_dir"],"*"))

        # Remove all files which match the skip file regex so they do not get copied
        filenames.delete_if { |f| File.expand_path(f).match(skip_files_regex) }

        FileUtils.cp_r filenames, cache_static_dir_path

        handler["expiration"] = expires_duration(handler["expiration"]) || default_expiration
      elsif handler["static_files"]

        # Need to convert all \1 into $1 so that nginx understands it
        handler["static_files"] = handler["static_files"].gsub(/\\/,"$")

        upload_regex = Regexp.new(handler["upload"])

        filenames = Dir.glob(File.join(untar_dir,"**","*"))

        filenames.each do |filename|
          relative_filename = get_relative_filename(filename,app_name)

          # Only include files that match the provided upload regular expression
          next if !relative_filename.match(upload_regex)

          # Skip all files which match the skip file regex so they do not get copied
          next if relative_filename.match(skip_files_regex)

          file_cache_path = File.join(cache_path, File.dirname(relative_filename))
          FileUtils.mkdir_p file_cache_path if !File.exists?(file_cache_path)
          
          FileUtils.cp_r filename, File.join(file_cache_path,File.basename(filename))
        end

        handler["expiration"] = expires_duration(handler["expiration"]) || default_expiration
      end
      handler
    end

    handlers.compact
  end

  # Parses the expiration string provided in the app.yaml and returns its duration in seconds
  def self.expires_duration input_string
    return nil if input_string.nil? || input_string.empty?
    # Start with nil so we can distinguish between it not being set and 0
    duration = nil
    input_string.split.each do |token|
      match = token.match(DELTA_REGEX)
      next if not match
      amount, units = match.captures
      next if amount.empty? || units.empty?
      duration = (duration || 0) + TIME_IN_SECONDS[units.downcase]*amount.to_i
    end
    duration
  end
end
