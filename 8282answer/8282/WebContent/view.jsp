<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="java.io.PrintWriter" %>
<%@ page import="bbs.Bbs" %>
<%@ page import="bbs.BbsDAO" %>
<%@ page import="t_answer.Answer" %>
<%@ page import="t_answer.AnswerDAO" %>
<%@ page import="t_member.UserDAO" %>
<%@ page import="t_member.User" %>
<%@ page import="java.util.ArrayList" %>
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta name="viewport" content="width=device-width", initial-scale="1">
<link rel="stylesheet" href="css/bootstrap.css">
<title>8282응답하라</title>
</head>
<body>
	<%
		String id = null;
		if (session.getAttribute("id") != null){
			id = (String) session.getAttribute("id");
		}
		int num_q=0;
		if (request.getParameter("num_q") != null) {
			num_q = Integer.parseInt(request.getParameter("num_q"));
		}
		if (num_q == 0) {
			PrintWriter script=response.getWriter();
			script.println("<script>");
			script.println("alert('유효하지 않은 글입니다.')");
			script.println("location.href='main.jsp'");
			script.println("</script>");
		}
		Bbs bbs = new BbsDAO().getBbs(num_q);
		
		session.setAttribute("num_q", num_q);
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
		<form method="post" action="viewAction.jsp">
		<table class="table table-striped" style="text-align: center; border: 1px solid #dddddd">
				<thead>
					<tr>
						<th colspan="3" style="background-color: #eeeeee; text-align: center; ">게시판 글 보기</th>
					</tr>
				</thead>
				<tbody>
				<%
					BbsDAO bbsDAO = new BbsDAO();	
				%>
					<tr>
						<td style="width: 20%;">글 제목</td>
						<td colspan="2"><%= bbs.getTitle() %><a onclick="return confirm('신고하시겠습니까?')" href="report_q.jsp?num_attacker=<%= bbs.getNum_m() %>" target="_blank" class="btn btn-primary pull-right">신고</a></td>
					</tr>
					<tr>
						<td>작성자</td>
						<td colspan="2"><%= bbsDAO.change(bbs.getNum_m())%></td>
					</tr>
					<tr>
						<td>작성일자</td>
						<td colspan="2"><%= bbs.getDate_q().substring(0, 11) + bbs.getDate_q().substring(11,13) + "시" + bbs.getDate_q().substring(14,16) + "분" %></td>
					</tr>
					<tr>
						<td>내용</td>
						<td colspan="2" style="min-height: 200px; text-align: left;"><%= bbs.getContent_q()%></td>
					</tr>
					<tr>
						<td>댓글작성</td>
						<td colspan="2"><textarea class="form-control" name="content_a" maxlength="1024"></textarea>
						<input type="submit" class="btn btn-primary pull-right" value="입력"></td>
					</tr>
				<%
					
					AnswerDAO answerDAO = new AnswerDAO();
					ArrayList<Answer> list = answerDAO.getList(num_q);
					for(int i = 0; i<list.size(); i++) {
						
						
				%>	
					<tr>
						<td><%= answerDAO.change(list.get(i).getNum_m()) %></td>
						<td colspan="2" style="text-align: left;"><%= list.get(i).getContent_a() %> (<%=list.get(i).getDate_a() %>) 
						추천 수 : <%= answerDAO.recocnt(list.get(i).getNum_a())%>
						<a onclick="return confirm('추천하시겠습니까?')" href="recommendAction.jsp?num_a=<%= list.get(i).getNum_a() %>">추천</a>
						<a onclick="return confirm('신고하시겠습니까?')" href="report_a.jsp?num_attacker=<%= list.get(i).getNum_m() %>" target="_blank" class="btn btn-primary pull-right">신고</a></td>
					</tr>
				<%	
					}
				%>				
				</tbody>
			</table>
			</form>
			<a href="main.jsp" class="btn btn-primary">목록</a>
		</div>
	</div>
	
	<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>
	<script src="js/bootstrap.js"></script>
</body>
</html>