<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="java.io.PrintWriter" %>
<%@ page import="bbs.BbsDAO" %>
<%@ page import="bbs.Bbs" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="t_member.UserDAO" %>
<%@ page import="t_member.User" %>
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta name="viewport" content="width=device-width", initial-scale="1">
<link rel="stylesheet" href="css/bootstrap.css">
<title>8282응답하라</title>
<style type="text/css">
	a, a:hover {
		color: #000000;
		text-decoration: none;
	}
</style>
</head>
<body>
	<%
		String id = null;
		if (session.getAttribute("id") != null){
			id = (String) session.getAttribute("id");
		}
		int pageNumber = 1;
		if (request.getParameter("pageNumber") != null) {
			pageNumber = Integer.parseInt(request.getParameter("pageNumber"));
		}

	%>

	<nav class="navbar navbar-default">
		<div class="navbar-header">
			<button type="button" class="navbar-toggle collapsed"
			data-toggle="collapse" data-target="#bs-example-navbar-collapse-1"
			aria-expanded="false">
				<span class="icon-bar"></span>
				<span class="icon-bar"></span>
				<span class="icon-bar"></span>
			</button>
			<a class="navbar-brand" href="main.jsp">8282 응,답하라</a>
		</div>
		<div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
			<ul class="nav navbar-nav">
				<li class="active"><a href="main.jsp">메인</a></li>				
			</ul>
			<%
				if (id == null) {
			%>
			<ul class="nav navbar-nav navbar-right">
				<li class="dropdown">
					<a href="#" class="dropdown-toggle"
					data-toggle="dropdown" role="button" aria-haspopup="true"
					aria-expanded="false">접속하기<span class="caret"></span></a>
					<ul class="dropdown-menu">
						<li><a href="login.jsp">로그인</a></li>
						<li><a href="join.jsp">회원가입</a></li>
					</ul>
				</li>
			</ul>
			<%
				} else {
					UserDAO userDAO = new UserDAO();
					User info = userDAO.getInfo(id);
					
					ArrayList<User> list1=userDAO.getList();
					int j=1;
					int rank=0;
					
					for(int i=0; i<list1.size(); i++){
						if(i!=0)
						{
							if(list1.get(i).getPoint() != list1.get(i-1).getPoint())
							{
							j++;
							}	
						}
						if(list1.get(i).getId().contains(id)){
							rank=j;
						}
					}

			%>
			<ul class="nav navbar-nav navbar-right">
				<li class="dropdown">
					<a href="#" class="dropdown-toggle"
					data-toggle="dropdown" role="button" aria-haspopup="true"
					aria-expanded="false">회원관리<span class="caret"></span></a>
					<ul class="dropdown-menu">
											
						<li>닉네임 : <%=info.getName() %></li>
						<li>포인트 : <%=info.getPoint() %></li>
						<li>등급 : <%=info.getGrade() %></li>
						<li>순위 : <%= rank %>
						
						</li>
					
						<li><a href="logoutAction.jsp">로그아웃</a></li>
						
					</ul>
				</li>
			</ul>
			<%		
				}
			%>
			
		</div>
	</nav>
	<div class="container">
		<div class="row">
			<table class="table table-striped" style="text-align: center; border: 1px solid #dddddd">
				<thead>
					<tr>
						<th style="background-color: #eeeeee; text-align: center; ">번호</th>
						<th style="background-color: #eeeeee; text-align: center; ">제목</th>
						<th style="background-color: #eeeeee; text-align: center; ">작성자</th>
						<th style="background-color: #eeeeee; text-align: center; ">작성일</th>
					</tr>
				</thead>
				<tbody>
					<%
						BbsDAO bbsDAO = new BbsDAO();
						ArrayList<Bbs> list = bbsDAO.getList4(pageNumber);
						for (int i=0; i<list.size(); i++) {
					%>
					<tr>
						<td><%= list.get(i).getNum_q() %></td>
						<td><a href="view.jsp?num_q=<%= list.get(i).getNum_q() %>"><%= list.get(i).getTitle() %></a></td>
						<td><%= bbsDAO.change(list.get(i).getNum_m()) %></td> 
						<td><%= list.get(i).getDate_q().substring(0, 11) + list.get(i).getDate_q().substring(11,13) + "시" + list.get(i).getDate_q().substring(14,16) + "분" %></td>
					</tr>
					<%		
						}
					%>
					
				</tbody>
			</table>
			<%
				if(pageNumber != 1) {
			%>
				<a href="Worry.jsp?pageNumber=<%=pageNumber - 1 %>" class="btn btn-sucess btn-arraw-left">이전</a>
			<%
				} if (bbsDAO.nextPage(pageNumber+1)) {
			%>	
				<a href="Worry.jsp?pageNumber=<%=pageNumber + 1 %>" class="btn btn-sucess btn-arraw-left">다음</a>
			<%
				}
			%>
			<a href="write.jsp" class="btn btn-primary pull-right">글쓰기</a>
			<br />
			<a href="family.jsp">가족</a>
			<a href="friendship.jsp">우정</a>
			<a href="love.jsp">사랑</a>
			<a href="Worry.jsp">고민</a>
			<a href="etc.jsp">기타</a>
			<br><br>
			<% if (id != null) { %>
			<a href="myquestion.jsp">작성한 게시글</a> <br>
			<a href="myanswer.jsp">답변한 게시글</a>
			<%
			}
			%>
			
			<br /><br />
			
		</div>
			<%
				UserDAO userDAO = new UserDAO();
				ArrayList<User> list1 = userDAO.getList();
				int j=1;
				for(int i=0; i<list1.size(); i++){
					if(i!=0){
					if(list1.get(i).getPoint() != list1.get(i-1).getPoint()){
						j++;
					}}
			%>
				
				<%= j %>.
				<%= list1.get(i).getName() %>
				<%= list1.get(i).getPoint() %>점<br/>
			<% } %>
			
			
				
		
	</div>
	<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>
	<script src="js/bootstrap.js"></script>
</body>
</html>