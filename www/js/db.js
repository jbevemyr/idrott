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

function layout_all_users(users) {
  for (var i = 0; i < users.length;  i++) {
    var row = $('<tr>');
    row.appendTo("#admin-funclist-table");
    row.append($('<td>').append(users[i].name));
    row.append($('<td>').append(users[i].group));
    row.append($('<td>').append(users[i].tel));
    row.append($('<td>').append(users[i].email));
    row.append($('<td>').append(users[i].comment));
  }
}

function layout_all_reqs(reqs) {
/*
          <li class="ui-field-contain">
            <label for="flip2">Flip switch:</label>
            <select name="flip2" id="flip2" data-role="slider">
                <option value="off">Off</option>
                <option value="on">On</option>
            </select>
        </li>
   Tanken är att använda detta för bekräftelse av event.
*/
}
