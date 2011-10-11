/* HTTP interface to JSHint.

   curl --form source="<path/to/my.js" --form=filename="my.js" http://127.0.0.1:3003/jshint

  TODO:
    parse incoming source files for embedded jshint options
    support file uploads?
    speed up
*/

var http = require('http'),
    formidable = require('formidable'),
    JSLINT = require('./jslint'),
    JSHINT = require('./jshint');

var hinters = {
  jshint: JSHINT.JSHINT,
  jslint: JSLINT.JSLINT
};

function getOpt(key) {
  var index = process.ARGV.indexOf(key);
  return index !== -1 ? process.ARGV[index + 1] : false;
}

function outputErrors(errors) {

  var e, i, output = [];

  function out(s) {
    output.push(s + '\n');
  }

  for (i = 0; i < errors.length; i += 1) {
    e = errors[i];
    if (e) {
      out('Lint at line ' + e.line + ' character ' + e.character + ': ' + e.reason);
      out((e.evidence || '').replace(/^\s*(\S*(\s+\S+)*)\s*$/, "$1"));
      out('');
    }
  }
  return output.join('');
}

function lintify(mode, sourcedata, filename) {
  var passed = hinters[mode](sourcedata, {});
  return passed ? "js: No problems found in " + filename + "\n"
    : outputErrors(hinters[mode].errors);
}

var port = getOpt("--port") || 3003,
    host = getOpt("--host") || "127.0.0.1";

http.createServer(function(req, res) {

  if (req.url === '/check' && req.method.toUpperCase() === 'POST') {
    var form = new formidable.IncomingForm();
    form.parse(req, function(err, fields, files) {
      var mode = (fields.mode && fields.mode == "jslint") ? "jslint" : "jshint";
      console.log('Applying \'' + mode + '\' to: ' + (fields.filename || 'anonymous'));
      var results = lintify(mode, fields.source, fields.filename);
      res.writeHead(200, {'Content-Type': 'text/plain'});
      res.end(results);
    });
    return;
  }

  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end("hello from jshint-mode");

}).listen(port, host);

console.log('Started JSHint server at http:// ' + host + ':' + port);