(function() {
  var Campfire;
  window.Campfire = Campfire = (function() {
    function Campfire(api_key, host) {
      this.url = "https://" + host + ".campfirenow.com/";
      this.auth = {
        'username': api_key,
        'password': 'X'
      };
      this.headers = {
        'Content-Type': 'application/json'
      };
    }
    Campfire.prototype.handlers = function(callbacks) {
      var resp;
      return resp = {
        onSuccess: function(response) {
          var obj;
          try {
            obj = JSON.parse(response.responseText);
          } catch (error) {
            console.dir(error);
            callbacks.onFailure(error);
          }
          return callbacks.onSuccess(obj);
        },
        onFailure: function(response) {
          console.dir(response);
          return callbacks.onFailure(response);
        }
      };
    };
    Campfire.prototype.rooms = function(callbacks) {
      return new Request(this.url, this.headers, this.auth).get('rooms.json', this.handlers(callbacks));
    };
    Campfire.prototype.roomInfo = function(id, callbacks) {
      return new Request(this.url, this.headers, this.auth).get("room/" + id + ".json", this.handlers(callbacks));
    };
    Campfire.prototype.recent = function(id, since, callbacks) {
      var url;
      url = "room/" + id + "/recent.json";
      if (since) {
        url += "?since_message_id=" + since;
      }
      return new Request(this.url, this.headers, this.auth).get(url, this.handlers(callbacks));
    };
    return Campfire;
  })();
}).call(this);
