package t_member;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

import bbs.Bbs;

public class UserDAO {

	private Connection conn;
	private PreparedStatement pstmt;
	private ResultSet rs;
	
	public UserDAO() {
		try {
			String dbURL="jdbc:mysql://localhost:3306/webple?characterEncoding=utf8&useUnicode=true&mysqlEncoding=utf8";
			String dbID="root";
			String dbPassword="1234";
			Class.forName("com.mysql.jdbc.Driver");
			conn=DriverManager.getConnection(dbURL,dbID,dbPassword);
		}catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public int login(String id, String password) {
		String SQL="select password from t_member where id=?";
		try {
			pstmt=conn.prepareStatement(SQL);
			pstmt.setString(1, id);
			rs=pstmt.executeQuery();
			if (rs.next()) {
				if(rs.getString(1).equals(password)) {
					return 1; //로그인 성공
				}
				else
					return 0; //비밀번호 불일치
			}
			return -1; //아이디가 없음
		}catch(Exception e) {
			e.printStackTrace();
		}
		return -2; //데이터베이스 오류
	}
	
	public void join(User user) {
		String SQL = "insert into t_member(name,id,password) values (?,?,?)";
		try {
			pstmt = conn.prepareStatement(SQL);
			pstmt.setString(1, user.getName());
			pstmt.setString(2, user.getId());
			pstmt.setString(3, user.getPassword());
			pstmt.executeUpdate();
		} catch(Exception e) {
			e.printStackTrace();
		}
		 //데이터베이스 오류
	}
	
	public boolean joinCheckId(String id) {
	      //user Id overlap check
	      String SQL = "select * from t_member where id = ?";
	      boolean check = false;
	      try {
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setString(1, id);
	         rs = pstmt.executeQuery();
	         check = rs.next();
	      } catch(Exception e) {
	         e.printStackTrace();
	      }
	      return check;
	   }
	
	public boolean joinCheckName(String name) {
	      //user Name overlap check
	      String SQL = "select * from t_member where name = ?";
	      boolean check = false;
	      try {
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setString(1, name);
	         rs = pstmt.executeQuery();
	         check = rs.next();
	      } catch(Exception e) {
	         e.printStackTrace();
	      }
	      return check;
	   }
	
	public String idname(String x) {
		String SQL=" select name from t_member where id=?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setString(1, x);
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return rs.getString(1);
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		return "";
	}
	
	public String numname(int x) {
		String SQL=" select name from t_member where num_m=?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, x);
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return rs.getString(1);
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		return "";
	}
	
	public void drop(int num_m) {
		String SQL="delete from t_member where num_m=?";
		try {         
			 PreparedStatement pstmt = conn.prepareStatement(SQL);
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setInt(1, num_m);
	         pstmt.executeUpdate();
	         System.out.println("test");
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
	}
	
	public void updateReport_a(int num_m){
		String SQL="update t_member set report_a = report_a +1  where num_m=?";
		try {         
			 PreparedStatement pstmt = conn.prepareStatement(SQL);
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setInt(1, num_m);
	         pstmt.executeUpdate();
	         
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
	}
	
	public void updateReport_q(int num_m){
		String SQL="update t_member set report_q = report_q +1  where num_m=?";
		try {         
			 PreparedStatement pstmt = conn.prepareStatement(SQL);
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setInt(1, num_m);
	         pstmt.executeUpdate();
	         
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
	}
	
	public ArrayList<User> reportList() {
		String SQL = "select * from t_member where report_q+report_a != 0 order by report_q+report_a desc";
		ArrayList<User> list = new ArrayList<User>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				User user = new User();
				user.setName(rs.getString(2));
				user.setReport_q(rs.getInt(7));
				user.setReport_a(rs.getInt(8));
				user.setNum_m(rs.getInt(1));
				list.add(user);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
		
	}
	
	public ArrayList<User> getList() {
		String SQL = "select * from t_member order by point desc limit 16";
		ArrayList<User> list1 = new ArrayList<User>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				User user = new User();
				user.setName(rs.getString(2));
				user.setId(rs.getString(3));
				user.setPoint(rs.getInt(5));
				list1.add(user);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list1;
		
	}

	public User getInfo(String id) {
	      //name, point, grade
	      String SQL = "select name, point, grade from t_member where id =?";
	      //select name, point, grade, (select count(*)+1 from t_member where point>t.point) as rank from t_member as t where id = ? order by rank asc;
	      User info = null;
	      
	      try {         
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setString(1, id);
	         rs = pstmt.executeQuery();
	         
	         if(rs.next()) {
	            info = new User();
	            info.setName(rs.getString("name"));
	            info.setPoint(rs.getInt("point"));
	            info.setGrade(rs.getInt("grade"));
	         }
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
	      return info;
	   }
	
	public int numatom(int num_a){
		String SQL="select num_m from t_answer where num_a=?";
		try {         
		 	 PreparedStatement pstmt = conn.prepareStatement(SQL);
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setInt(1, num_a);
	         rs=pstmt.executeQuery();
	         if (rs.next()) {
					return rs.getInt(1);
				}
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
		return -1;
	}
	public void inputpoint(int num_m){
		String SQL="update t_member set point = point +3  where num_m=?";
		try {         
			 PreparedStatement pstmt = conn.prepareStatement(SQL);
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setInt(1, num_m);
	         pstmt.executeUpdate();
	         
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
	}
	
	public void qpoint(String id){
		String SQL="update t_member set point = point -20  where id=?";
		try {         
			 PreparedStatement pstmt = conn.prepareStatement(SQL);
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setString(1, id);
	         pstmt.executeUpdate();
	         
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
	}
	
	public void apoint(String id){
		String SQL="update t_member set point = point +10  where id=?";
		try {         
			 PreparedStatement pstmt = conn.prepareStatement(SQL);
	         pstmt = conn.prepareStatement(SQL);
	         pstmt.setString(1, id);
	         pstmt.executeUpdate();
	         
	      }catch(Exception e) {
	         e.printStackTrace();
	      }
	}
	
	
	
	
}
