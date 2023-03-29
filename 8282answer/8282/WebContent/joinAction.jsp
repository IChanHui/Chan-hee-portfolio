<%@page import="com.sun.xml.internal.txw2.Document"%>
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@ page import="t_member.UserDAO" %>
<%@ page import="recommend.RecommendDAO" %>
<%@ page import="java.io.PrintWriter" %>
<% request.setCharacterEncoding("UTF-8"); %>
<jsp:useBean id="user" class="t_member.User" scope="page"/>
<jsp:setProperty name="user" property="id" />
<jsp:setProperty name="user" property="password" />
<jsp:setProperty name="user" property="name" />

<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>8282응답하라</title>
</head>
<body>
	<%
	request.setCharacterEncoding("UTF-8");
    String pwd1 = request.getParameter("password");
    String pwd2 = request.getParameter("password2");
    String id=null;
    if (session.getAttribute("id") != null ){
 id = (String) session.getAttribute("id");
    }      
    
    if (id != null) {
 PrintWriter script=response.getWriter();
 script.println("<script>");
 script.println("alert('이미 로그인이 되어있습니다.')");
 script.println("location.href='main.jsp'");
 script.println("</script>");
    }
 
    if (user.getId() == null || user.getName() == null || pwd1 == "" || pwd2 == "") {
 PrintWriter script=response.getWriter();
 script.println("<script>");
 script.println("alert('입력이 안 된 사항이 있습니다.')");
 script.println("history.back()");
 script.println("</script>");
    } else {
       UserDAO userDAO = new UserDAO();
       boolean checkId = userDAO.joinCheckId(user.getId());
       boolean checkName = userDAO.joinCheckName(user.getName());
       
       if (!checkId && !checkName) {
          //Id, Name false
          //join seccess
          if (user.getName().length() <= 10) {
             //Name.length <= 10
             if (pwd1.equals(pwd2)) {
                //pwd1 , pwd2 check true
                userDAO.join(user);
                session.setAttribute("id",user.getId());
                PrintWriter script = response.getWriter();
                script.println("<script>");
                script.println("alert('회원가입이 완료되었습니다!')");
                script.println("location.href = 'main.jsp'");
                script.println("</script>");
             } else if (!pwd1.equals(pwd2)) {
                //pwd1, pwd2 check false
                PrintWriter script = response.getWriter();
                script.println("<script>");
                script.println("alert('비밀번호가 일치하지 않습니다. 다시 입력해 주세요.')");
                script.println("history.back()");
                script.println("</script>");
             }//if~else pwd1, pwd2 check
          }//if Name.length
       }//if check Id, Name

       if (checkName) {
          //Name true
          PrintWriter script = response.getWriter();
          script.println("<script>");
          script.println("alert('이미 존재하는 닉네임 입니다.')");
          script.println("history.back()");
          script.println("</script>");
       }

       if (checkId) {
          //Id true
          PrintWriter script = response.getWriter();
          script.println("<script>");
          script.println("alert('이미 존재하는 아이디 입니다.')");
          script.println("history.back()");
          script.println("</script>");
       }

       if (user.getName().length() > 10) {
          //Name.length check
          PrintWriter script = response.getWriter();
          script.println("<script>");
          script.println("alert('닉네임을 10자 내외로 입력해 주세요.')");
          script.println("history.back()");
          script.println("</script>");
       }

       if (user.getId().length() >= 100) {
          //Id.length check
          PrintWriter script = response.getWriter();
          script.println("<script>");
          script.println("alert('아이디를 100자 내외로 입력해 주세요.')");
          script.println("history.back()");
          script.println("</script>");
       }

       if (pwd1.length() > 100 || pwd2.length() > 100) {
          //pwd1, pwd2 .length check
          PrintWriter script = response.getWriter();
          script.println("<script>");
          script.println("alert('비밀번호를 100자 내외로 입력해 주세요.')");
          script.println("history.back()");
          script.println("</script>");
       }
    }
		
		
	%>
</body>
</html>