function findSession() {
    var sid = $.trim($("input[name='session-id']")[0].value)
    var qp = ""
    if ($("input[name='media-details']")[0].checked)
        qp = "?details=media"
    resetContent()
    $.ajax({
        url: "/api/calls/" + sid + "/trace" + qp,
        method: "GET",
        dataType: "json",
        success: function(res) {
            console.debug(res)
            if (res["diagram"]) {
                setAliases(res["flows"].concat(res["channels"]))
                makeDiagram(res["diagram"], res["channels"])
                    .drawSVG("res-graph", {theme: "simple"})
                $("#res-status").html("")
            } else {
                $("#res-status").html("Could not find the session " + sid)
            }

        },
        error: function(xhr, status, errorThrown) {
            $("#res-status").html("Error connecting to stern: " + status)
        }
    })
}

function setAliases(as) {
    _.each(as, function(a) {
        $("#res-aliases").append("<dt>" + a[0] + "</dt><dd>" + a[1] + "</dd>")
    })
}

function makeDiagram(ps, cs) {
    var diagram = new Diagram()
    var actors = []
    for (var i = 0; i < cs.length; i++) {
        actors.push(new Diagram.Actor(cs[i][0], cs[i][0], i))
    }
    actors.push(new Diagram.Actor("belfry", "Belfry", actors.length))
    diagram.actors = actors
    _.each(ps, function(p) {
        diagram.addSignal(toSignal(diagram, p))
    })
    return diagram
}

function toSignal(d, p) {
    switch (p["type"]) {
        case "line":
            var ls = p["dashSpacing"] == 0.0 ?  0 : 1
            var as = p["arrowStyle"] == "openArrow" ? 4 : 0
            return new Diagram.Signal(d.getActor(p["fromActor"]),
                                      ls + as,
                                      d.getActor(p["toActors"][0]),
                                      p["label"])
        case "note":
            return new Diagram.Note(d.getActor(p["actors"][0]), 2, p["text"])
    }
}

function resetContent() {
    $("#res-aliases").html("")
    $("#res-graph").html("")
}
