function event(user) {
    return {status: "ja", evenemang: "SAYO (dag 2)", date: "2014/06/06"};
}

function get_all_users(sid, cont) {
    alert("in function get_all_users");
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

/*
http://idrott/idrott/get_all_events?sid=1234567890

        {status: "ok", events: [ <event> ]}
        {status: "error", reason: <reason>}

http://idrott/idrott/create_event?sid=1234567890
POST json object for event, ie POST:ing the json object
{ "name": <name>, "date": <date>}
will create an event with those attributes

        {status: "ok", event: <event>}
        {status: "error", reason: <reason>}

*/

function get_all_events(sid, cont) {
    //alert("in function get_all_events");
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
  var eventheading = $('<h4>').append(event.name);
  var bemanning = "Bemanning";
  var eventbody = $('<p>').append(bemanning);
  eventbody.append($('<p>').append("<strong>Datum: </strong>"+event.date));
  eventbody.append($('<p>').append("<strong>Plats: </strong>"+event.location));
  eventbody.append($('<p>').append("<strong>Funktionärsanmälan: </strong>"+event.funccall));
  eventbody.append($('<p>').append("<strong>Antal funktionärer: </strong>"+event.funccount));
  eventbody.append($('<p>').append("<strong>Funktionärsinfo: </strong>"+event.funcinfo));
  var x = $('<div>').attr({ 'data-role': 'collapsible', 'id' : rowid });
  x.append(eventheading);
  x.append(eventbody);
  return x;
}


function add_new_event(sid, event) {
  $.post("http://idrott.bevemyr.com/idrott/create_event?sid="+sessionid,
	 JSON.stringify({
           name: $('#es-name').value,
           date: $('#es-date').value,
           location: $('#es-location').value,
           funccount: $('#es-funccount').value,
           funccall: $('#es-funccall').value
	 }),
	 function(status) {
	   if(status.status == "ok") { // string otherwise an object session/group
             //add_result("  "+JSON.stringify(status.event));
             //test_event_id = status.event.id;
             //add_result("---- ok");
             //run_tests();
		
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
   Tanken är att använda detta för bekräftelse av event.
*/
}
