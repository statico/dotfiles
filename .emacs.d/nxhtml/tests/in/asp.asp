
<%@LANGUAGE="VBScript"%>

<SCRIPT LANGUAGE="JavaScript" RUNAT="Server">
function JSGreeting()
	{
	return "Greetings from a JavaScript Function";
	}
</SCRIPT>

<SCRIPT LANGUAGE="VBScript" RUNAT="Server">
Function VBGreeting()
	VBGreeting="Greetings from a VBScript Function"
End Function

Function toDollars(x)
	toDollars=FormatCurrency(x)
End Function
</SCRIPT>

<%
var a = 2;
var b = 2;
var c = add(a,b)
c += " (Two numbers are added by JavaScript, "
c += "and then formatted into currency by VBScript.)"

function add(x,y)
	{
	result = x + y;
	result = toDollars(result);
	return result;	
	}

Response.Write("<HTML>\r")
Response.Write(JSGreeting() + "<BR>\r")
Response.Write(VBGreeting() + "<BR>\r")
Response.Write(c + " <BR>\r")
Response.Write("</HTML>\r")
%>
