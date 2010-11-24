In JSP page, if we have a line like <% // some comments %>, then the "%>" is ignored by nxhtml.
However if we write it like this:
<% // some comments
%>
or
<%
// some comments
%>
then the page is parsed correctly
