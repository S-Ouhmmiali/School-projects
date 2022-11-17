<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>

<form action= "Controler" method="get">
login <input type="text" name="login"><br/>
<br/>
password <input type="text" name="mdp"><br/>
<input type="submit" name="ajouter" value="connect as a manager">
<input type="hidden" name="op" value="ConnectionManager">
<% String message = (String)request.getAttribute("message"); %>
<% if(message != null) { %>
	<%=message%>
 <% } %>
 
 <a href="index.html">Retour au Menu</a>
</form>

</body>
</html>