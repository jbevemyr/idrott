function event(user) {
    return {status: "ja", evenemang: "SAYO (dag 2)", date: "2014/06/06"};
}

function get_all_users(sid, cont) {
    alert("in function");
//return [{user: "johan", email: "jb@bevemyr.com", tel: "070123"}];
    $.ajax({
        url: "http://idrott.bevemyr.com/idrott/get_all_users?sid="+sid,
	dataType: "json",
	success: function(data) {
	       cont(data.users);
	},
	error: function(data) {
	    alert(data.status);
	    $.mobile.changePage($("#login"));  // should be errorpage
	}
    });

}
