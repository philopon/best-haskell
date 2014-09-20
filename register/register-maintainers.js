var MongoClient = require('mongodb').MongoClient;
var request     = require('request');
var url         = process.env.MONGOHQ_URL;

function getMaintainers(pkgs, succ) {
  var ret = {};
  function req (pkg, next) {
    console.log(pkg);
    request.get({url: 'http://hackage.haskell.org/package/'+pkg+'/maintainers/', json: true}, function(err,rsp,bdy){
        var ms = []
        if (!err && rsp.statusCode == 200) {
          for (var i = 0; i < bdy.members.length; i++) {
            ms.push(bdy.members[i].username);
          }
          ret[pkg] = ms;
        }
        if (next.length == 0) {
          succ(ret);
        } else {
          req(next[0], next.slice(1));
        }
    });
  }
  req(pkgs[0], pkgs.slice(1));
}


MongoClient.connect(url, function(err, db){
  var col = db.collection('packages');

  col.find({maintainers: {$exists: false}}, {name: 1}).toArray(function(err,res){
    var pkgs = [];
    for(var i = 0; i < res.length; i++){
      pkgs.push(res[i].name);
    }
    console.log(pkgs.length);

    var bulk = col.initializeUnorderedBulkOp();
    getMaintainers(pkgs, function(ms){
      for(var pkg in ms) {
        bulk.find({name: pkg}).updateOne({$set: {maintainers: ms[pkg]}});
      }

      bulk.execute(function(err, result){
        db.close();
      });

    });

  });

});

/*

    MongoClient.connect(url, function(err, db){
      var col  = db.collection('packages');
      var bulk = col.initializeUnorderedBulkOp();

      var nUpd = 0;
      var first = new Date(last - 1000 * 60 * 60 * 24 * 31);

      for(var pkg in downloads) {
        var recent  = [];
        var history = [];
        var total   = 0;
        var initial = undefined;
        for(var date in downloads[pkg]) {
          var d = new Date(date);
          total += downloads[pkg][date];
          var p = {date: d, count: downloads[pkg][date]};
          if (d >= first) {recent.push(p);}
          if (!initial || initial > d) { initial = d; }
          history.push(p);
        }
        nUpd++;
        bulk.find({name: pkg}).updateOne({$set: {recent: recent, downloads: history, initialRelease: initial, total: total}});
      }

      console.log(nUpd);

      if(nUpd > 0) {
        bulk.execute(function(err, result){
          if(err) { console.log(err); db.close(); process.exit(); }
          db.collection('config').update({key: 'last_update'}, {key: 'last_update', value: last}, {upsert: true}, function(err, result){
            if(err) { console.log(err); db.close(); process.exit(); }
            db.collection('config').update({key: 'recent_start'}, {key: 'recent_start', value: first}, {upsert: true}, function(err, result){
              if (err)  { console.log(err); db.close(); process.exit(); } else { db.close() }
            });
          });
        });
      }
    });
  });
  */
