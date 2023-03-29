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
<link rel="stylesheet" href="css/home.css">
<title>8282 응, 답하라</title>
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
    <video autoplay muted loop poster="background.mp4" background-size=cover no-respect>
        <source src="video/background.mp4" type="video/mp4">
    </video>

    <table align="center">  
        <thead>
            <tr>
                <th colspan="2" align="left" title="그 시절이 떠오르는 이곳에서 고민 해결하세요^^">
                    <img src="images/logo.png">
                </th>
                <th></th>
            </tr>
        </thead>
        <tbody>
            <tr>
                 <td id="ranking" rowspan="2" text-align="left"><b>
                     <div id="rkh">★ Ranking ★</div>
                      <div>1. 웹플팀 9000점 <img src="images/air1.svg"></div>
                      <div>2. 웹플팀입니다 8000점 <img src="images/air2.svg"></div>
                      <div>3. 구앱플현웹플이 7000점 <img src="images/air3.svg"></div>
                      <div>4. 졸업작품 6000점 <img src="images/air4.svg"></div>
                      <div>1. 웹플팀 9000점 <img src="air5.svg"></div>
                      <div>2. 웹플팀입니다 8000점 <img src="air6.svg"></div>
                      <div>3. 구앱플현웹플이 7000점 <img src="air.svg"></div>
                      <div>4. 졸업작품 6000점 <img src="air.svg"></div>
                      <div>1. 웹플팀 9000점 <img src="air.svg"></div>
                      <div>2. 웹플팀입니다 8000점 <img src="air.svg"></div>
                      <div>3. 구앱플현웹플이 7000점 <img src="air.svg"></div>
                      <div>4. 졸업작품 6000점 <img src="air.svg"></div>
                      <div>1. 웹플팀 9000점 <img src="air.svg"></div>
                      <div>2. 웹플팀입니다 8000점 <img src="air.svg"></div>
                      <div>3. 구앱플현웹플이 7000점 <img src="air.svg"></div>
                      <div>16. 졸업작품 6000점 <img src="air.svg"></div>
                 </b></td>

                  <td id="main" rowspan="2" align="center">
                    <button type="button" id="ref"  class="hover">질문하러 GO
                        <span class="hovertitle">로그인해야 할 수 있어요😊 ⇒</span>
                    </button>
                        <button type="button" id="post">
                            <div id="sp1">제목입니다제목입니다제목입니다제목다제목입니다</div><br>
                            <div id="sp2">내용입니다내용입니다내용입니다내용다</div>
                        </button><br><br>
                        <button type="button" id="post">
                            <div id="sp1">제목입니다제목입니다제목입니다제입니다</div><br>
                            <div id="sp2">내용입니다내용입니다내용입입니다내용입니다내용입니다</div>
                        </button><br><br>
                        <button type="button" id="post">
                            <div id="sp1">제목입니다제목입니다제목입</div><br>
                            <div id="sp2">내용입니다내용입니다내용입니니다내용입니다</div>
                        </button><br><br>
                        <button type="button" id="post">
                            <div id="sp1">제목입니다제목입니다제목입다제목입니다제목입니다</div><br>
                            <div id="sp2">내용입니다내용입니다내용입니니다내용입니다내용입니다내용입니다</div>
                        </button><br><br>
                        <button type="button" id="post">
                            <div id="sp1">제목입니다제목입니다제목입니다제목입니다</div><br>
                            <div id="sp2">내용입니다내용입니다내용입니다내입니다내용입니다내용입니다</div>
                        </button><br><br>
                        <button type="button" id="post">
                            <div id="sp1">제목입니다제목입니다제목입니다제목입니다제목입니다제목입니다제목입니다제목입니다</div><br>
                            <div id="sp2">내용입니다내용입니다내용입니다내용입니다내용입니다내용입니다내용입니다내용입니다내용입니다내용입니다</div>
                        </button>
                    <br><button type="button" id="bdown">▼</button>
                  </td>
                  <%
					if (id == null) {
					%>
                  <td id="home">
                    <button type="button" onclick="location.href='login.jsp'" title="로그인"><b>&emsp; Login &emsp;</b></button><br><br>
                    <button type="button" onclick="location.href='join.jsp'" title="회원가입"><b>Membership</b></button>
                  </td>
                  <%
					} else {
						UserDAO userDAO = new UserDAO();
						User info = userDAO.getInfo(id);
						ArrayList<User> list1 = userDAO.getList();
						int rank = 0;
						
						for(int i = 0; i<list1.size(); i++) {
							if(list1.get(i).getId().contains(id))
			                  {
								rank = i+1;
			                  }
					}
			%>
			<td id="home">
                    <span id="hrank"><%=rank %></span>
                    <img id="himg" src="air.svg"><%=info.getGrade() %><br><!-- Grade 기능 손보기 -->
                    <div id="nick">남산 칠봉이</div><br>
                    <div id="score"><%=info.getPoint() %></div><br>
                    <button type="button" onclick="location.href='Home.html'" title="로그아웃">
                        Logout
                    </button>
                  </td>
            <%		
				}
			%>
            </tr>
            <tr>
                <td id="category">
                    <li><a href="#"><b>#가족</b></a></li>
                    <li><a href="#"><b>#사랑</b></a></li>
                    <li><a href="#"><b>#우정</b></a></li>
                    <li><a href="#"><b>#고민</b></a></li>
                    <li><a href="#"><b>#기타</b></a></li><!--<br><br>
                    <li><a href="#">☞ 작성한 게시글</a></li>
                    <li><a href="#">☞ 답변한 게시글</a></li>-->
                </td>
            </tr>
        </tbody>

        <tfoot>
            <tr>
                <td colspan="3">
                    ☆ BAEWHA Women's University ☆<br >
                    @웹플팀 : 소유진, 유지선, 이찬희, 이시연
                </td>
            </tr>
        </tfoot>
    </table>

</body>
</html>