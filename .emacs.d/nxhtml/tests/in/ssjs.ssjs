<p id="msg"></p>
<script runat="server">
  var nme = document.createTextNode(
      "Hello my name is Jaxer.");
  var para = document.getElementById("name");
  para.appendChild(nme);
</script>

<p>Hello, my name is <% response.name %>.</p>
