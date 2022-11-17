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
<% Collection<Post> pc = (Collection<Post>) request.getAttribute("listePosts");
Collection<Campagne> cc = (Collection<Campagne>) request.getAttribute("listeCampagnes");
%>
<h1> Posts : </h1> <br>
<% for(Post p : pc){
     String nomm = p.getTitre() + " " + p.getUrl();
     int idpo = p.getId();
%>
<input type="radio" name = "idpost" value="<%=idpo %>" >
<%=nomm %> <br>

<%}
%>
<br>
<br>
<h1> Campagnes : </h1> <br>
<% for(Campagne c : cc){
     String nomc = c.getNom()+" "+ c.getContexte()+" "+ c.getObjectif();
     int idc = c.getId();
%>
<input type="radio" name = "idcampagne" value="<%=idc %>" >
<%=nomc %> <br>

<%}
%>
<input type="submit" name="op_associer" value="confirmer">
<input type="hidden" name="op" value="associerPostCampagne">
 <a href="managerConnect.html">Retour au Menu</a>
</form>
</body>
</html>