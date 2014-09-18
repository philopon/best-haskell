var MongoClient = require('mongodb').MongoClient;
var fs = require('fs');

var url  = process.env.MONGOHQ_URL
var json = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));

MongoClient.connect(url, function(err, db){
  var col = db.collection('packages');
  var bulk = col.initializeUnorderedBulkOp();

  col.find().toArray(function(err, docs){
    var nMod = 0;
    for(var i = 0; i < docs.length; i++){
      var doc = docs[i];
      if (json[doc.name].version !== doc.version) {
        bulk.find({name: doc.name}).updateOne({$set: json[doc.name]});
        nMod++;
      }
      delete json[doc.name];
    }
    console.log("modified: " + nMod);

    var nNew = 0;
    for(var key in json) {
      json[key]['name'] = key;
      bulk.insert(json[key]);
      nNew++;
    }
    console.log("new: " + nNew);

    if(nMod + nNew > 0) {
      bulk.execute(function(err, result){
        if(err){console.log(err);}
        db.close();
      });
      } else {
        db.close();
      }
  });
});
