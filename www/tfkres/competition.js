/**
 * Created by katrin on 14-10-30.
 */

$(function () {

    var Gren = Backbone.Model.extend({
        name: "",
        startlistURL: "",
        resURL: ""
    });
    var Grenar = Backbone.Collection.extend({
        model: Gren
    });
    var g = new Grenar("60m");

    var Race = Backbone.Model.extend({
        group: "",
        subgroup: "",
        grenar: []
    });
    var Races = Backbone.Collection.extend({
        model: Race,
        url: 'competition.json'
        //url: '/TFKfunk/www/tfkres/race.json'
    });
    /*
    var races = new Races({
        "group": "Pojkar",
        "subgroup": "02",
        "grenar": gs
    });
    */
    var races = new Races();
    races.fetch({async: false});

    var Competition = Backbone.Model.extend({
        name: "",
        races: []
    });

    var comp = new Competition({
        "name": "Sayo Indoor 2015",
        "races": races
    });


    var NavView = Backbone.View.extend({
        el: $("#nav"),

        resultTemplate: _.template($('#nav-list-template').html()),

        render: function () {
            var groups = [],
                headplace = $(this.el),
                resulttemplate = this.resultTemplate;

            //console.log("collection: "+this.collection.toJSON());
            var grouplist = this.collection.map(function (x) {
                return x.toJSON().group;
            });
            grouplist = _.unique(grouplist);
            //console.log(grouplist);

            for (var i=0; i < grouplist.length; i++)
            {
                var group = grouplist[i];
                console.log(group);

                var subgrouplist = this.collection.filter(function (x) {
                    return x.toJSON().group == group;
                });

                var subgroupstringlist = subgrouplist.map(function (x) {
                    return x.toJSON().subgroup;
                });

                var navlist = _.reduce(subgrouplist, function (memo, x) {
                    return memo + x;
                });

                var subgroupfilter = _.map(subgrouplist, function (x){
                    return x.toJSON().subgroup;
                });

                subgroupfilter = _.reduce(subgroupfilter, function (memo, x) {
                    return memo + x;
                });
                console.log(subgroupfilter);

                var groupfilter = group + " " + subgroupfilter;

                groups.push({group: group, groupfilter: groupfilter, subgroups: subgroupstringlist});
                console.log("group: "+group+", groupfilter: "+groupfilter+", subgroups: "+subgroupstringlist[2]);
            }

            var navstruct = resulttemplate({groups: groups});
            //var navstruct = resulttemplate({group: "TESTGROUP", groupfilter: "TESTGROUPFILTER", subgroup: "TESTSUBGROUP"});
            console.log(navstruct);
            headplace.append(navstruct);
            return this;
        }
    });

    var ResView = Backbone.View.extend({
        el: $("#res-app"),

        resultpageTemplate: _.template($('#res-page-template').html()),

        render: function () {
            var pagetemplate = this.resultpageTemplate;

            //this.footer.html(this.statsTemplate({done: done, remaining: remaining}));
            $(this.el).append(pagetemplate());
            return this;
        }
    });

    var page = new ResView;
    page.render();

    var navigation = new NavView({collection: races});
    navigation.render();
});