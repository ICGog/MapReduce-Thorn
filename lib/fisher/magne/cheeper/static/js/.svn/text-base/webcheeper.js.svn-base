var ui = {};

ui.showMessage = function(message){
    $('#message').hide().html(message).fadeIn();  
};

ui.clearMessage = function(){
    $('#error').html('');
};

ui.formError = function(xhr, textStatus, errorThrow){
    ui.showMessage(xhr.responseText);
};

/**
 * Cheep form
 */
ui.cheepFormValidate = function(formData, jqForm, options){
    ui.clearMessage();    
    var cheep = $('#cheep').val();
    
    if(cheep === ""){
        ui.showMessage("Dude cheap is empty!");
        return false;
    }
    return true;
};

ui.cheepFormSuccess = function(data, textStatus, xhr){
    $('#cheepForm').clearForm();
    cheep = jQuery(data).hide();
    $('#cheeps').prepend(cheep);
    cheep.slideDown();
    var number = $('#cheepsnumber');
    number.fadeOut(function(){
                       number.text(parseInt(number.text()) + 1).fadeIn();
                   });
    $('#latest_version').val(ui.latestCheepVersion());
};

ui.latestCheepVersion = function(){
    var version = $('#cheeps li:first')[0];
    // cheep-time- length 11
    version = (version ? version.id.substr(11) : "0");
    return version;
};

ui.cheepForm = function(){
  
    $('#cheepForm').ajaxForm(
        {
            beforeSubmit: ui.cheepFormValidate,
            success: ui.cheepFormSuccess,
            error: ui.formError
        });

    
    function update(){
        $.ajax({
                   url: '/cheeps/',
                   data: { latest_version: ui.latestCheepVersion() },
                   success: function(data, textStatus, xhr){
                       cheep = jQuery(data).hide();
                       $('#cheeps').prepend(cheep);
                       cheep.slideDown();
                       var number = $('#cheepsnumber');
                       $('#latest_version').val(ui.latestCheepVersion());
                   }
               });
    }

    window.setInterval(update, 5000);

    $('#latest_version').val(ui.latestCheepVersion());
};

/**
 * Log in form
 */
ui.loginFormValidate = function(formData, jqForm, options){
    var username = $('#username').val(),
    password = $('#password').val();

    username = $.trim(username);
    password = $.trim(password);
    
    if (username === "" || password === "") {
        ui.showMessage("Please suply username and password.");
        return false;
    }
    return true;
};


ui.loginFormSuccess = function(data, textStatus, form){
    window.location.href = data;
};

ui.loginForm = function(){
    $('#loginForm').ajaxForm(
        {
            beforeSubmit: ui.loginFormValidate,
            error: ui.formError,
            success: ui.loginFormSuccess
        });
};

ui.settingsFormSuccess = function(responseText, statusText, xhr, $form){
    var result = eval('(' + responseText + ')');
    ui.showMessage(result.text);
    if(result.status === 200){
        if($('input:radio[name=updateavatar]:checked').val() === "true"){
            $('#avatarimg').fadeOut('slow', function(){
                                        var uri = result.avatar + "?" + Math.random();
                                        $("#avatarimg").html('<img height="32" src="' + uri + '" alt="avatar" />').fadeIn();
                                    });
        }

    }
};

/**
 *  Settings form
 */
ui.settingsForm = function(){
    $('#settingsForm').ajaxForm(
        {
            error: ui.formError,
            success: ui.settingsFormSuccess
        });

    $('input:radio[name=twitterintegration]').click(
        function(){
            var e = $('#twitterlogin'); 
            this.value !== "no" ? e.fadeIn() : e.fadeOut();
        });

    $('input:radio[name=twittercheeperlogin]').click(
        function(){
            var e = $('#twitterlogininput'); 
            this.value === "false" ? e.fadeIn() : e.fadeOut();
        }
    );

    $('input:radio[name=updateavatar]').click(
        function(){
            var e = $('#avatar'); 
            this.value === "true" ? e.fadeIn() : e.fadeOut();
        }
    );
};

/**
 * Signup form
 */ 
ui.signupForm = function(){
    $('#signupForm').ajaxForm(
        {
            beforeSubmit: ui.loginFormValidate,
            error: ui.formError,
            success: ui.loginFormSuccess
        }
    );
};

/**
 * (Un)Follow form
 */
ui.followFormSuccess = function(data, textStatus, xhr, form){
    var li = form.closest('li');
    li.replaceWith(jQuery(data));
    ui.unfollowForms();    
};

ui.unfollowFormSuccess = function(data, textStatus, xhr, form){
    var li = form.closest('li');
    li.replaceWith(jQuery(data));
    ui.followForms();
};

ui.followForms = function(){
    $('.followForm').ajaxForm({ success: ui.followFormSuccess, error: ui.formError });  
};

ui.unfollowForms = function(){
    $('.unfollowForm').ajaxForm({ success: ui.unfollowFormSuccess, error: ui.formError });  
};

function main(){
    $('#loginForm').length && ui.loginForm();        
    $('#cheepForm').length && ui.cheepForm();        
    $('#signupForm').length && ui.signupForm();        
    $('.followForm').length && ui.followForms();        
    $('.unfollowForm').length && ui.unfollowForms();        
    $('#settingsForm').length && ui.settingsForm();
};