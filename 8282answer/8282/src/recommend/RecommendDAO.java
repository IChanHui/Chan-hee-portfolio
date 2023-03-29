package recommend;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import recommend.Recommend;

public class RecommendDAO {

	private Connection conn;
	private PreparedStatement pstmt;
	private ResultSet rs;
	
	public RecommendDAO() {
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
	
	public void input(int num_a,String id){
		String SQL = "insert into recommend values (?,?,?)";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, num_a);
			pstmt.setString(2, id);
			pstmt.setInt(3, 1);
			pstmt.executeUpdate();
			}catch (Exception e){
			e.printStackTrace();
		}
		
	}
	
	
	
	
	
	public int getvalue(int num_a,String id){
		String SQL = "select value from recommend where num_a=? and id=?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, num_a);
			pstmt.setString(2, id);
			rs=pstmt.executeQuery();
			if (rs.next()) {
				return rs.getInt(1);
			}
		}catch (Exception e){
			e.printStackTrace();
		}
		return -1;
	}
	
	public void valueup(int num_a, String id) {
		String SQL = "update recommend set value=1 where num_a=? and id=?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, num_a);
			pstmt.setString(2, id);
			pstmt.executeUpdate();
			
		}catch (Exception e){
			e.printStackTrace();
		}

	}
	
	
}
