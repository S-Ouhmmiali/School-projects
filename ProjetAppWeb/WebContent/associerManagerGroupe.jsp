<%@ page language="java" import="pack.*, java.util.*" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Insert title here</title>
</head>
<body>
<form action= "Controler" method="get">
<% Collection<Manager> lm = (Collection<Manager>) request.getAttribute("listeManagers");
Collection<Groupe> lg = (Collection<Groupe>) request.getAttribute("listeGroupes");
%>
<h1> Managers : </h1> <br>
<% for(Manager m : lm){
	 String nomm = m.getNom() + " " + m .getPrenom();
	 int idm = m.getId();
%>
<input type="radio" name = "idmanager" value="<%=idm %>" >
<%=nomm %> <br>

<%}
%>
<br>
<br>
<h1> Groupes : </h1> <br>
<% for(Groupe g : lg){
	 String nomg = g.getNom();
	 int idg = g.getId();
%>
<input type="radio" name = "idgroupe" value="<%=idg %>" >
<%=nomg %> <br>

<%}
%>
<input type="submit" name="op_associer" value="confirmer">
<input type="hidden" name="op" value="associerManagerGroupe">
 <a href="managerConnect.html">Retour au Menu</a>
</form>
</body>
</html>