/**
 * Created by katrin on 14-10-30.
 */

$(function () {

    var Race = Backbone.Model.extend({
        group: "",
        subgroup: "",
        events: []
    });
    var Races = Backbone.Collection.extend({
        model: Race
    });

    var Competition = Backbone.Model.extend({
        //name: "",
        //date: "",
        //races: []
    });
    var Competitions = Backbone.Collection.extend({
        model: Competition,
        url: 'competition.json'
    });

    var NavView = Backbone.View.extend({
        el: $("#nav"),

        resultTemplate: _.template($('#nav-grouplist-template').html()),

        render: function () {
            var groups = [],
                headplace = $(this.el),
                resulttemplate = this.resultTemplate;

            var grouplist = this.collection.map(function (x) {
                return _.property("group")(x);
            });
            grouplist = _.unique(grouplist);

            for (var i=0; i < grouplist.length; i++)
            {
                var group = grouplist[i];

                var subgrouplist = this.collection.filter(function (x) {
                    return _.property("group")(x) == group;
                });

                var subgroupstringlist = subgrouplist.map(function (x) {
                    return _.property("subgroup")(x);
                });

                var navlist = _.reduce(subgrouplist, function (memo, x) {
                    return memo + x;
                });

                var subgroupfilter = _.map(subgrouplist, function (x){
                    return _.property("subgroup")(x);
                });

                subgroupfilter = _.reduce(subgroupfilter, function (memo, x) {
                    return memo + x;
                });

                var groupfilter = group + " " + subgroupfilter;

                groups.push({group: group, groupfilter: groupfilter, subgroups: subgroupstringlist});
            }

            var navstruct = resulttemplate({groups: groups})
            headplace.append(navstruct);
            return this;
        }
    });

    var ResView = Backbone.View.extend({
        el: $("#res-app"),

        resultpageTemplate: _.template($('#res-page-template').html()),

        render: function () {
            var pagetemplate = this.resultpageTemplate;

            $(this.el).append(pagetemplate());
            return this;
        }
    });

    var comps = new Competitions();
    comps.fetch({async: false});

    var page = new ResView;
    page.render();

    //console.log("antal tävlingsdagar: "+comps.length);

    if (comps.length > 0) {
        var races = comps.at(0).get("races");
        var navigation = new NavView({collection: races});
        navigation.render();
    };


});