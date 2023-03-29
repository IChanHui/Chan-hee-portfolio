package bbs;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;

public class BbsDAO {
	private Connection conn;
	private ResultSet rs;
	
	public BbsDAO() {
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
	
	public String getDate() {
		String SQL = "SELECT NOW()";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return rs.getString(1);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return ""; //데이터베이스 오류
	}
	
	public int getNext() {
		String SQL = "SELECT num_q from T_QUESTION order by num_q desc";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return rs.getInt(1) + 1;
			}
			return 1; // 첫 번째 게시물인 경우
		} catch (Exception e) {
			e.printStackTrace();
		}
		return -1; //데이터베이스 오류
	}
	
	public int write(String title, int num_m, String content_q, int category) {
		String SQL = "insert into t_question values (?,?,?,?,?,?)";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext());
			pstmt.setInt(2, num_m);
			pstmt.setString(3, title);
			pstmt.setString(4, content_q);
			pstmt.setInt(5, category); //카테고리,,
			pstmt.setString(6, getDate());
			return pstmt.executeUpdate();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return -1; //데이터베이스 오류
	}
	
	public int idnum(String x) {
		String SQL=" select num_m from t_member where id=?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setString(1, x);
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return rs.getInt(1);
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		return -1;
	}
	
	public String change(int y) {
		String SQL=" select distinct name from t_member as m join t_question as q on m.num_m = q.num_m where m.num_m = ?;";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, y);
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return rs.getString(1);
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		return "-1";
	}
	
	public ArrayList<Bbs> getList(int pageNumber) {
		String SQL = "select * from t_question where num_q < ? order by num_q desc limit 10";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> getList1(int pageNumber) {
		String SQL = "select * from t_question where num_q< ? and category=0  order by num_q desc limit 10;";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> getList2(int pageNumber) {
		String SQL = "select * from t_question where num_q< ? and category=1  order by num_q desc limit 10;";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> getList3(int pageNumber) {
		String SQL = "select * from t_question where num_q< ? and category=2  order by num_q desc limit 10;";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> getList4(int pageNumber) {
		String SQL = "select * from t_question where num_q< ? and category=3  order by num_q desc limit 10;";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> getList5(int pageNumber) {
		String SQL = "select * from t_question where num_q< ? and category=4  order by num_q desc limit 10;";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> getList6(int pageNumber, int num_m) {
		String SQL = "select * from t_question where num_q< ? and num_m=?  order by num_q desc limit 10;";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10);
			pstmt.setInt(2, num_m);
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> getList7(int num_q) {
		String SQL = "select * from t_question where num_q= ? ";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, num_q);
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public ArrayList<Bbs> list_m(int num_m) {
		String SQL = "select distinct num_q from t_answer where num_m=?";
		ArrayList<Bbs> list = new ArrayList<Bbs>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, num_m); 
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				list.add(bbs);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
	
	
	
	public boolean nextPage(int pageNumber) {
		String SQL = "select * from t_question where num_q < ?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, getNext() - (pageNumber - 1) * 10); 
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return true;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}
	
	public Bbs getBbs(int num_q) {
		String SQL = "select * from t_question where num_q = ?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, num_q); 
			rs = pstmt.executeQuery();
			if (rs.next()) {
				Bbs bbs = new Bbs();
				bbs.setNum_q(rs.getInt(1));
				bbs.setNum_m(rs.getInt(2));
				bbs.setTitle(rs.getString(3));
				bbs.setContent_q(rs.getString(4));
				bbs.setCategory(rs.getInt(5));
				bbs.setDate_q(rs.getString(6));
				return bbs;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
}
