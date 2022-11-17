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
<input type="submit" name="ajouter" value="ConnectUser">
<input type="hidden" name="op" value="ConnectionUser">
<a href="Register.html"> Créer un compte</a> 

<br/><% String message = (String)request.getAttribute("message"); %>
<% if(message != null) { %>
	<%=message%>
 <% } %><br/>
 <a href="index.html">Retour au Menu</a>
</form>



</body>
</html>