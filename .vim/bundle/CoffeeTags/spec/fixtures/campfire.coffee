# this example is not complete, but shows how to
# implement an API client using Request class
class Campfire

  # @api_key - Campfire API keys
  # @host - your campifre host, ie if you're using trololo.campfirenow.com,
  # then host is 'trololo
  constructor: (api_key, host) ->
    @url = "https://#{host}.campfirenow.com/"
    @auth = { 'username' : api_key, 'password' : 'X'}
    @headers = { 'Content-Type' : 'application/json' }

  # private function used for parsing JSON responses
  handlers: (callbacks) ->
    resp =
      onSuccess : (response) ->
        try
          obj = JSON.parse(response.responseText)
        catch error
          console.dir(error)
          callbacks.onFailure(error)
        callbacks.onSuccess(obj)

      onFailure: (response) ->
        console.dir(response)
        callbacks.onFailure(response)

  # get list of rooms
  rooms: (callbacks) ->
    new Request(@url, @headers, @auth).get 'rooms.json', this.handlers(callbacks)

  # get information about a room
  # @id - room id
  roomInfo: (id, callbacks) ->
    new Request(@url, @headers, @auth).get "room/#{id}.json", this.handlers(callbacks)

  # get latest messages and events from a room
  # @id - room id
  # @since - optional since id parameter
  recent: (id, since, callbacks) ->
    url = "room/#{id}/recent.json"
    url += "?since_message_id=#{since}" if since
    new Request(@url, @headers, @auth).get url, this.handlers(callbacks)

class Test
  bump : ->
