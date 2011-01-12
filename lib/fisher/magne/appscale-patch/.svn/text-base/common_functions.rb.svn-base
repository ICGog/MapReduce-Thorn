#!/Usr/bin/ruby -w
# Programmer: Chris Bunch

require 'digest/sha1'
require 'net/http'
require 'openssl'
require 'open-uri'
require 'socket'
require 'timeout'
require 'yaml'

EMAIL_REGEX = /\A[[:print:]]+@[[:print:]]+\.[[:print:]]+\Z/
PASSWORD_REGEX = /\A[[:print:]]{6,}\Z/
IP_REGEX = /\d+\.\d+\.\d+\.\d+/
FQDN_REGEX = /[\w\d\.\-]+/
IP_OR_FQDN = /#{IP_REGEX}|#{FQDN_REGEX}/

APP_YAML_CONFIG = "app.yaml"
WEB_XML_CONFIG = "war/WEB-INF/appengine-web.xml"

module CommonFunctions
  # cgb: added in shell function for backticks so that we can unit test it
  # since flexmock doesn't like backticks since its name is non-alphanumeric
  # e.g., its name is Kernel, :`
  def self.shell(command)
    `#{command}`
  end

  def self.clear_app(app_path, force=false)
    return if !File.exists?(app_path)
    return if app_path !~ /\A\/tmp/ and !force
    remove_me = app_path.scan(/(\A.*)\//).flatten.to_s
    FileUtils.rm_rf(remove_me, :secure => true)
  end

  def self.validate_appname(app_name)
    disallowed = ["none", "auth", "login", "new_user", "load_balancer"]
    disallowed.each { |not_allowed|
      abort("App can't be called '#{not_allowed}'") if app_name == not_allowed 
    }
    abort("App name can only contain alphanumerics and .-@") if app_name =~ /[^[:alnum:].@-]/
    return app_name
  end
  
  def self.get_ips_from_yaml(ips)
    return "using_tools" if ips.nil?

    ips_to_use = []
    if !ips[:servers].nil?
      ips[:servers].each { |ip|
        if ip =~ IP_REGEX
          ips_to_use << ip
        else
          ips_to_use << CommonFunctions.convert_fqdn_to_ip(ip)
        end
      }
      ips_to_use = ips_to_use.join(":")
    end
    
    return ips_to_use
  end
  
  def self.get_credentials(testing)
    if testing
      return "a@a.a", "aaaaaa"
    else
      return CommonFunctions.get_email, CommonFunctions.get_password
    end
  end

  def self.wait_until_redirect(host, url_suffix)
    uri = "http://#{host}#{url_suffix}"
    loop {
      response = ""
      begin
        response = Net::HTTP.get_response(URI.parse(uri))
      rescue Exception => e
        abort("[unexpected] We were unable to see if your app is running. We saw an exception of type #{e.class}")
      end
      
      return if response['location'] != "http://#{host}/status"
      sleep(1)
    }
  end

  def self.user_has_cmd?(command)
    output = CommonFunctions.shell("which #{command}")
    if output == ""
      return false
    else
      return true
    end
  end

  def self.convert_fqdn_to_ip(host)
    nslookup = CommonFunctions.shell("nslookup #{host}")
    ip = nslookup.scan(/#{host}\nAddress:\s+(#{IP_REGEX})/).flatten.to_s
    abort("Couldn't convert #{host} to an IP address. Result of nslookup was \n#{nslookup}") if ip.nil? or ip == ""
    return ip
  end

  def self.encrypt_password(user, pass)
    salted = user + pass
    Digest::SHA1.hexdigest(salted)
  end

  def self.sleep_until_port_is_open(ip, port, use_ssl=false)
    loop {
      return if CommonFunctions.is_port_open?(ip, port, use_ssl)
      Kernel.sleep(1)
    }
  end

  def self.sleep_until_port_is_closed(ip, port, use_ssl=false)
    loop {
      return unless CommonFunctions.is_port_open?(ip, port, use_ssl)
      Kernel.sleep(1)
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
        rescue Exception
          return false
        end
      end
    rescue Timeout::Error
    end
  
    return false
  end

  def self.run_remote_command(ip, command, public_key_loc, want_output)
    if public_key_loc.class == Array
      public_key_loc.each { |key|
        key = File.expand_path(key)
      }
      
      remote_cmd = "ssh -i #{public_key_loc.join(' -i ')} -o StrictHostkeyChecking=no 2>&1 root@#{ip} '#{command}"
    else
      public_key_loc = File.expand_path(public_key_loc)
      remote_cmd = "ssh -i #{public_key_loc} -o StrictHostkeyChecking=no root@#{ip} '#{command} "
    end
    
    if want_output
      remote_cmd << "> /tmp/#{ip}.log 2>&1 &' &"
    else
      remote_cmd << "> /dev/null 2>&1 &' &"
    end

    Kernel.system remote_cmd
    return remote_cmd
  end
  
  def self.find_real_ssh_key(ssh_keys, host)
    ssh_keys.each { |key|
      key = File.expand_path(key)
      return_value = CommonFunctions.shell("ssh -i #{key} -o NumberOfPasswordPrompts=0 -o StrictHostkeyChecking=no 2>&1 root@#{host} 'touch /tmp/foo'; echo $?").chomp
      return key if return_value == "0"
    }
    
    return nil
  end

  def self.scp_file(local_file_loc, remote_file_loc, target_ip, public_key_loc)
    cmd = ""
    local_file_loc = File.expand_path(local_file_loc)
    
    if public_key_loc.class == Array
      public_key_loc.each { |key|
        key = File.expand_path(key)
      }
      
      cmd = "scp -i #{public_key_loc.join(' -i ')} -o StrictHostkeyChecking=no 2>&1 #{local_file_loc} root@#{target_ip}:#{remote_file_loc}"
    else
      public_key_loc = File.expand_path(public_key_loc)
      cmd = "scp -i #{public_key_loc} -o StrictHostkeyChecking=no 2>&1 #{local_file_loc} root@#{target_ip}:#{remote_file_loc}"
    end

    cmd << "; echo $? >> ~/.appscale/retval"

    retval_loc = File.expand_path("~/.appscale/retval")
    FileUtils.rm_f(retval_loc)

    begin
      Timeout::timeout(10) { CommonFunctions.shell("#{cmd}") }
    rescue Timeout::Error
      abort("Remotely copying over files failed. Is the destination machine on and reachable from this computer? We tried the following command:\n\n#{cmd}")
    end

    loop {
      break if File.exists?(retval_loc)
      sleep(5)
    }

    retval = (File.open(retval_loc) { |f| f.read }).chomp
    abort("\n\n[#{cmd}] returned #{retval} instead of 0 as expected. Is your environment set up properly?") if retval != "0"
    return cmd
  end

  def self.get_email
    email = nil
    puts "\nThis AppScale instance is linked to an e-mail address giving it administrator privileges."
    
    loop {
      print "Enter your desired administrator e-mail address: "
      STDOUT.flush
      new_email = STDIN.gets.chomp
      print "Please repeat your e-mail address to verify: "
      STDOUT.flush
      verify_email = STDIN.gets.chomp
      
      if new_email == verify_email
        email = new_email
        
        if email =~ EMAIL_REGEX
          break
        else
          puts "The response you typed in was not an e-mail address. Please try again.\n\n"
        end
      else
        puts "E-mail addresses entered do not match. Please try again.\n\n"
      end
    }
    
    return email
  end

  def self.get_password
    pass = nil
    puts "\nThe new administrator password must be at least six characters long and can include non-alphanumeric characters."
    
    loop {
      system "stty -echo" # Turn off character echoing
      print "Enter your new password: "
      STDOUT.flush
      new_pass = STDIN.gets.chomp
      print "\nEnter again to verify: "
      STDOUT.flush
      verify_pass = STDIN.gets.chomp
      system "stty echo" # Next release: find a platform independent solution
      
      if new_pass == verify_pass
        pass = new_pass
        
        if pass =~ PASSWORD_REGEX
          break
        else
          puts "\n\nThe password you typed in was not at least six characters long. Please try again.\n\n"
        end
      else
        puts "\n\nPasswords entered do not match. Please try again.\n\n"
      end
    }
    
    return pass
  end
  
  def self.get_from_yaml(keyname, tag, required=true)
    location_file = File.expand_path("~/.appscale/locations-#{keyname}.yaml")
  
    abort("An AppScale instance is not currently running with the provided keyname, \"#{keyname}\".") unless File.exists?(location_file)  
    
    begin
      tree = YAML.load_file(location_file)
    rescue ArgumentError
      if required
        abort("The yaml file you provided was malformed. Please correct any errors in it and try again.")
      else
        return nil
      end
    end
    
    value = tree[tag]
    
    bad_yaml_format_msg = "The file #{location_file} is in the wrong format and doesn't contain a #{tag} tag. Please make sure the file is in the correct format and try again"
    abort(bad_yaml_format_msg) if value.nil? and required
    return value
  end

  def self.get_load_balancer_ip(keyname, required=true)
    return CommonFunctions.get_from_yaml(keyname, :load_balancer)
  end
  
  def self.get_load_balancer_id(keyname, required=true)
    return CommonFunctions.get_from_yaml(keyname, :instance_id)  
  end

  def self.get_table(keyname, required=true)
    return CommonFunctions.get_from_yaml(keyname, :table, required)
  end

  def self.write_node_file(head_node_ip, instance_id, table)
    tree = { :load_balancer => head_node_ip, :instance_id => instance_id , :table => table }
    loc_path = File.expand_path(LOCATIONS_YAML)
    File.open(loc_path, "w") {|file| YAML.dump(tree, file)}
  end

  def self.get_random_alphanumeric(length=10)
    random = ""
    possible = "0123456789abcdefghijklmnopqrstuvxwyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    possibleLength = possible.length

    length.times { |index|
      random << possible[rand(possibleLength)]
    }

    return random
  end

  def self.get_appname_from_tar(fullpath)
    appname, file, language = CommonFunctions.get_app_info(fullpath, APP_YAML_CONFIG)

    if appname.nil? or file.nil? or language.nil?
      appname, file, language = CommonFunctions.get_app_info(fullpath, WEB_XML_CONFIG)
    end

    if appname.nil? or file.nil? or language.nil?
      abort("We could not find a valid app.yaml or web.xml file in your application.")
    end

    return appname, file, language
  end

  def self.get_app_info(fullpath, app_file)
    abort("AppEngine upload tar file not found") unless File.exists?(fullpath)
    filename = fullpath.scan(/\/?([\w\.]+\Z)/).flatten.to_s
    temp_dir = CommonFunctions.get_random_alphanumeric
    FileUtils.rm_rf("/tmp/#{temp_dir}", :secure => true)
    FileUtils.mkdir_p("/tmp/#{temp_dir}")

    begin
      FileUtils.cp(fullpath, "/tmp/#{temp_dir}/#{filename}")
    rescue Errno::EACCES
      abort("Copying the file to /tmp failed")
    end

    FileUtils.rm_f("/tmp/#{temp_dir}/#{app_file}")
    

    tar_file = CommonFunctions.shell("cd /tmp/#{temp_dir}; tar zxvfm #{filename} 2>&1; echo $?").chomp

    tar_ret_val = tar_file.scan(/\d+\Z/).to_s

    abort("Untar'ing the given tar file in /tmp failed") if tar_ret_val != "0"

#    verbose("Looking for #{app_file} in tar #{tar_file}")

    #possible_app_yaml = tar_file.scan(/[\s\.\w\d\-\/]*\/?(#{app_file})/)
    
    possible_app_yaml = tar_file.scan(/#{app_file}/)

#    verbose("after scanning #{possible_app_yaml}")

    if possible_app_yaml.length == 0
      FileUtils.rm_rf("/tmp/#{temp_dir}", :secure => true)
      return nil, nil, nil
    elsif possible_app_yaml.length == 1
      app_yaml_loc = possible_app_yaml.to_s
    else
      # does the current regex prevent this from being reached?
      failure_message = "The given AppEngine tar file contains more than one #{app_file} file. Please remove them, tar up your app, and try again. We saw the following #{app_file} files in your project:\n" + possible_app_yaml.join("\n")
      FileUtils.rm_rf("/tmp/#{temp_dir}", :secure => true)
      abort(failure_message + "\ntarfile:" + tar_file)
    end
    

    if app_file == APP_YAML_CONFIG
      appname, language = CommonFunctions.get_appname_and_runtime_via_yaml(temp_dir, app_yaml_loc)
      file = fullpath
    elsif app_file == WEB_XML_CONFIG
      appname = CommonFunctions.get_appname_via_xml(temp_dir, app_yaml_loc)
      language = "java"
      FileUtils.rm_rf("/tmp/#{temp_dir}/war/WEB-INF/lib/", :secure => true)
      FileUtils.mkdir_p("/tmp/#{temp_dir}/war/WEB-INF/lib")
      temp_dir2 = CommonFunctions.get_random_alphanumeric
      FileUtils.rm_rf("/tmp/#{temp_dir2}", :secure => true)
      FileUtils.mkdir_p("/tmp/#{temp_dir2}")
      FileUtils.rm_f("/tmp/#{temp_dir}/#{filename}")
      CommonFunctions.shell("cd /tmp/#{temp_dir}; tar -czf ../#{temp_dir2}/#{appname}.tar.gz .")
      file = "/tmp/#{temp_dir2}/#{appname}.tar.gz"
    else
      FileUtils.rm_rf("/tmp/#{temp_dir}", :secure => true)
      abort("appname was #{app_file}, which was not a recognized value.")
    end

    if appname.nil?
      FileUtils.rm_rf("/tmp/#{temp_dir}", :secure => true)
      abort("AppEngine tar file is invalid - Doesn't have an app name in #{app_file}")
    end

    FileUtils.rm_rf("/tmp/#{temp_dir}", :secure => true)
    return appname, file, language 
  end

  def self.get_appname_and_runtime_via_yaml(temp_dir, app_yaml_loc)
    app_yaml_loc = "/tmp/" + temp_dir + "/" + app_yaml_loc
    
    begin
      tree = YAML.load_file(app_yaml_loc.chomp)
    rescue ArgumentError
      abort("The yaml file you provided was malformed. Please correct any errors in it and try again.")
    end
    
    appname = tree["application"]
    runtime = tree["runtime"]
    return appname, runtime
  end

  def self.get_appname_via_xml(temp_dir, xml_loc)
    xml_loc = "/tmp/" + temp_dir + "/" + xml_loc
    web_xml_contents = (File.open(xml_loc) { |f| f.read }).chomp
    appname = web_xml_contents.scan(/<application>([\w\d-]+)<\/application>/).flatten.to_s
    appname = nil if appname == ""
    return appname
  end
end
