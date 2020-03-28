const MongoClient = require('mongodb').MongoClient;
const assert = require('assert');

// Connection URL
const url_DB = 'mongodb+srv://loladmin:jeanDB@loldb-9slby.mongodb.net/test?retryWrites=true&w=majority';

// Database Name
const dbName = 'Champions';

// Create a new MongoClient
const client = new MongoClient(url_DB);

// -------  Partie 1 -------


const findDocuments = function(db, col, query, callback) {
  // Get the documents collection
  const collection = db.collection(col);
  // Find some documents
    collection.find(query).toArray(function(err, docs) {
	assert.equal(err, null);
	//console.log("Found the following records");
	//console.log(docs)
	callback(docs);
  });
}

// Use connect method to connect to the Server
client.connect(function(err) {
    assert.equal(err, null);
    console.log("Connected correctly to server");
    const db = client.db(dbName);
    findDocuments(db, 'fulldata', {},function(data){
	//console.log('data', data);
    });
});



// -------  Partie 2 -------




var http = require('http');
var url = require('url');
var querystring = require('querystring');
var fs = require('fs');
var express = require('express');
var app = express();

app.get('/*', function(req, res) {
    var page = url.parse(req.url).pathname;
    var params = querystring.parse(url.parse(req.url).query);

    fs.readFile(__dirname + '/site/' + page,
            function(err, data) {
                if (err) {
                    res.writeHead(500);
                    return res.end('Error loading ' + page);
                }
                console.log('sending page ' + page);
                res.end(data);
            });

    // sending response OK
});

var port = 8080;
console.log("listening to " + port);


var io = require('socket.io').listen(app.listen(port), {log: true});

// when the client is ready
io.sockets.on('connection', function(socket) {
    socket.on('ready', function(data) {
        console.log('received', 'ack');
        const db = client.db(dbName);
        findDocuments(db, 'fulldata', {}, function(docs){
                var champ = {"items" : docs};
            socket.emit("data", champ);
        });
    });
    socket.on('sent', function(data) {
        console.log('received', data);
        socket.emit("message", data);
    });
    socket.on('selected', function(data) {
        console.log('received', data);
        const db = client.db(dbName);
        findDocuments(db, 'fulldata', {"name":data.name}, function(doc){
            console.log("found",doc[0].name);
            socket.emit("details", doc);
        });
    });
});
