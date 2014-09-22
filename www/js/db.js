

function get_all_users(sid, cont, tablename) {
    $.ajax({
        url: "http://idrott.bevemyr.com/idrott/get_all_users?sid="+sid,
	dataType: "json",
	success: function(data) {
	       cont(sid, tablename, data.users);
	},
	error: function(data) {
	    alert(data.status);
	    $.mobile.changePage($("#login"));  // should be errorpage
	}
    });
}

function layout_all_users(sid, tablename, users) {
  $(tablename+" tbody").empty();
  var groups = [];
  var row = "";
  var x = "";

  for (var i = 0; i < users.length;  i++) {
    x = "<a href=\"#admin-add-user-to-event\" data-rel=\"popup\" class=\"ui-btn ui-icon-bullets ui-btn-icon-notext ui-corner-all ui-mini\" data-transition=\"flip\" onclick=\"return true;\"></a>";

    if (groups.indexOf((users[i].group) < 0) || (users[i].group != "")) {
      groups.push(users[i].group);
    }

    row = $('<tr>');
    row.appendTo(tablename);
    row.append($('<td>').append(users[i].name));
    row.append($('<td>').append(users[i].group));
    row.append($('<td>').append(users[i].tel));
    row.append($('<td>').append(users[i].email));
    row.append($('<td>').append(users[i].comment));
    row.append($('<td>').append(x));
  }

  //$("#admin-adminlist-table").table("rebuild");
  //alert("Groups: "+groups);
  get_all_events(sid, layout_select_event);
}


// G�r man s� h�r l�gger dem i sekvens eller ska de triggas med event, t ex att man trycker p� knappen.
// Kanske r�cker det att man resettar valet i dialogen.
function layout_select_event(events) {
  var domObj = "#admin-add-user-to-event-event";
  //var item = $('<ledgend>').append("Arrangemang:").appendTo(domObj);
  var eid;
  var ename;
  $(domObj).empty();
  for (var i = 0; i < events.length; i++) {
    eid = events[i].id;
    ename = events[i].name;
    $('<input type="radio" name="' + eid + '" id="event' + eid +'"><label for="event' + eid + '">' + ename + '</label>').appendTo(domObj);
    $('div').trigger('create');
  }
  //$('[type="radio"]').trigger("create");
}

function connect_user_to_event(sid, uid) {
  alert("Connect "+uid);
  
}

function get_admin_users(sid, tablename) {
  $(tablename+" tbody").empty();
  $.post("http://idrott.bevemyr.com/idrott/get_selected_users?sid="+sid,
	    JSON.stringify({
	      role: "admin",
	      confirmed: false
	    }),
	    function(data) {
	      if(data.status == "ok") {
		layout_admin_users(data.users);
              } else {
		alert(data.status);
		$.mobile.changePage($("#login"));  // should be errorpage
	      }
	    },
	 "json"
  );
}

function layout_admin_users(users) {
  for (var i = 0; i < users.length;  i++) {
    var row = $('<tr>');
    row.appendTo("#admin-adminlist-table");
    row.append($('<td>').append(users[i].name));
    row.append($('<td>').append(users[i].tel));
    row.append($('<td>').append(users[i].email));
    row.append($('<td>').append(users[i].comment));
  }
}

function add_new_user(sid) {
  $.post("http://idrott.bevemyr.com/idrott/add_user?sid="+sid,
	 JSON.stringify({
           name: $('#us-name').val(),
           username: $('#us-email').val(),
	   password: "test",
           email: $('#us-email').val(),
           tel: $('#us-phone').val(),
	   group: $('#us-group').val(),
           role: $('#us-role').val(),
           events: [],
	   confirmed: false,
	   comment: $('#us-not').val()
	 }),
	 function(status) {
	   if(status.status == "ok") { // string otherwise an object session/group
	     $('#us-form')[0].reset();
	     $.mobile.changePage($("#admin-funclist"));
           } else {
	     alert(status.status);
	     $.mobile.changePage($("#login"));
	   }
	 },
	 "json"
  );
}

function update_user(sid) {
  $.post("http://idrott.bevemyr.com/idrott/add_user?sid="+sid,
	 JSON.stringify({
           name: $('#us-update-name').val(),
           email: $('#us-update-email').val(),
           phone: $('#us-update-phone').val(),
	   group: $('#es-update-group').val(),
           role: $('#es-update-role').val(),
           events: [],  //TBD
	   not: $('#es-not').val()
	 }),
	 function(status) {
	   if(status.status == "ok") { // string otherwise an object session/group
	     $('#us-update-form')[0].reset();
	     $.mobile.changePage($("#admin-eventlist"));
           } else {
	     alert(status.status);
	     $.mobile.changePage($("#login"));
	   }
	 },
	 "json"
  );
}


function get_all_events(sid, cont) {
    $.ajax({
        url: "http://idrott.bevemyr.com/idrott/get_all_events?sid="+sid,
	dataType: "json",
	success: function(data) {
	       cont(data.events);
	},
	error: function(data) {
	    alert(data.status);
	    $.mobile.changePage($("#login"));  // should be errorpage
	}
    });
}

function layout_all_events(events) {
  // Ta bort de gamla raderna
  $('#admin-eventlist-table').empty();
  for (var i = 0; i < events.length;  i++) {
    var rowid = "admin-eventlist-table-row"+i;
    var row = layout_eventrow(events[i], rowid);
    $('#admin-eventlist-table').append(row).trigger("create").collapsibleset("refresh");
  }
}

function layout_eventrow(event, rowid) {
  var row = $('<div>').attr({ 'data-role': 'collapsible', 'id' : rowid });
  row.append($('<h4>').append(event.name));
  row.append($('<p>').append("<a href='#admin-event-funclist'>Bemanning</a>"));
  row.append($('<p>').append("<strong>Datum: </strong>"+event.date));
  row.append($('<p>').append("<strong>Plats: </strong>"+event.location));
  row.append($('<p>').append("<strong>PM: </strong>"+event.pm));
  row.append($('<p>').append("<strong>Tidsprogram: </strong>"+event.timeschedule));
  row.append($('<p>').append("<strong>Funktionsärsinfo: </strong>"+event.funcinfo));
  row.append($('<p>').append("<strong>Antal funktionärer: </strong>"+event.funccount));
  row.append($('<p>').append("<strong>Funktionärsanmälan: </strong>"+event.funccall));
  row.append($('<p>').append("<strong>Funktionärsinfo: </strong>"+event.funcinfo));

  return row;
}


function add_new_event(sid) {
  $.post("http://idrott.bevemyr.com/idrott/create_event?sid="+sid,
	 JSON.stringify({
           name: $('#es-name').val(),
           date: $('#es-date').val(),
           location: $('#es-loc').val(),
	   pm: $('#es-pm').val(),
	   timeschedule: $('#es-ts').val(),
	   funcinfo: $('#es-fi').val(),
           funccount: $('#es-fn').val(),
           funccall: $('#es-fcall').val(),
	   not: $('#es-not').val()
	 }),
	 function(status) {
	   if(status.status == "ok") { // string otherwise an object session/group
	     $('#es-form')[0].reset();
	     $.mobile.changePage($("#admin-eventlist"));
           } else {
	     //add_result("---- fail: "+status.reason);
	     //run_tests();
	   }
	 },
	 "json"
  );
}

function get_all_func_for_event(sid, eid, tablename) {
  $.post("http://idrott.bevemyr.com/idrott/get_selected_users?sid="+sid,
	 JSON.stringify({
           events: [{eventid: eid}]
	 }),
	 function(data) {
	   if(data.status == "ok") { // string otherwise an object session/group
	     layout_event_funclist(tablename, data.users);
           } else {
	     alert(data.status);
	     $.mobile.changePage($("#login"));	     
	   }
	 },
	 "json"
  );
}

function layout_event_funclist(tablename, users) {
  // You want to be able to print the list and export to csv.
  var groups = [];
  var groupoptions = "";
  var row = "";
  $(tablename+" tbody").empty();
  groupoptions.appendTo("#admin-event-funclist-groups");
  for (var i = 0; i < users.length;  i++) {
    row = $('<tr id=funclistID='+i+'>');
    row.appendTo(tablename);
    row.append($('<td>').append(users[i].group));
    row.append($('<td>').append(users[i].name));
    row.append($('<td>').append(users[i].tel));
    row.append($('<td>').append(users[i].email));
    row.append($('<td>').append(users[i].comment));
    row.append($('<td>').append());

    // <option value="small">One</option>
    groupoptions.append($('<option>').append(users[i].group));
    groups.push(users[i].group);
  }
  alert(groups);
}



function update_event(sid) {
  $.post("http://idrott.bevemyr.com/idrott/create_event?sid="+sid,
	 JSON.stringify({
           name: $('#es-update-name').val(),
           date: $('#es-update-date').val(),
           location: $('#es-update-location').val(),
	   pm: $('#es-update-pm').val(),
	   timeschedule: $('#es-update-ts').val(),
	   funcinfo: $('#es-update-fi').val(),
           funccount: $('#es-update-funccount').val(),
           funccall: $('#es-update-funccall').val(),
	   not: $('#es-update-not').val()
	 }),
	 function(status) {
	   if(status.status == "ok") { // string otherwise an object session/group
	     $('#es-update-form')[0].reset();
	     $.mobile.changePage($("#admin-eventlist"));
           } else {
	     //add_result("---- fail: "+status.reason);
	     //run_tests();
	   }
	 },
	 "json"
  );
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
   Tanken �r att anv�nda detta f�r bekr�ftelse av event.
*/
}
