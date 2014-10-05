var csv = require('csv');
var MongoClient = require('mongodb').MongoClient;
var fs  = require('fs');

var url  = process.env.MONGOHQ_URL

var downloads = {};
var versions  = {};
fs.createReadStream(process.argv[2], 'utf8').pipe(csv.parse())
  .on('data', function(data){
    var pkg     = data[0];
    var date    = data[1];
    var version = data[2];
    var count   = parseInt(data[3]);
    if (!downloads[pkg]) {downloads[pkg] = {};}
    if (!downloads[pkg][date]) {downloads[pkg][date] = 0}
    if (!versions[pkg])          {versions[pkg] = {};}
    if (!versions[pkg][version]) {versions[pkg][version] = date; }
    downloads[pkg][date] += count;
  })
  .on('end', function(){
    var last = new Date(0);

    for(var pkg in downloads) {
      for(var date in downloads[pkg]) {
        var d = new Date(date);
        if (d > last) { last = d; }
      }
    }

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

        var releases = [];
        var lastRel = undefined;
        for(var ver in versions[pkg]) {
          var d = new Date(versions[pkg][ver]);
          releases.push({version: ver, release: d});
          if (!lastRel || lastRel < d) { lastRel = d; }
        }

        nUpd++;
        bulk.find({name: pkg}).updateOne({$set: {recent: recent, downloads: history, initialRelease: initial, lastRelease: lastRel, total: total, releases: releases}});
      }

      console.log(last);
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
