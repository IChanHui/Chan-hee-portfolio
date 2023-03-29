package t_stop;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

public class StopDAO {

	private Connection conn;
	private PreparedStatement pstmt;
	private ResultSet rs;
	
	public StopDAO() {
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
	
	public void stop(int num_m, int type) {
		String SQL = "insert into t_stop values (?,?,?,?)";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, num_m);
			pstmt.setInt(2, type);
			pstmt.setString(3, getDate());
			pstmt.setInt(4, 0);
			pstmt.executeUpdate();
		} catch (Exception e){
			e.printStackTrace();
		}
	}
	
	//질문,답변 작성 정지 일주일 지나면 풀림
	public void deleteStop_1() {
		String SQL = "delete from t_stop where date(date) < date(subdate(now(), interval 7 DAY)) and type <= 1";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.executeUpdate();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	//계정정지 30일 지나면 풀림
	public void deleteStop_2() {
		String SQL = "delete from t_stop where date(date) < date(subdate(now(), interval 30 DAY)) and type >= 2";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.executeUpdate();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
	
	public int confirm(int num_m,int type) {
		String SQL = "select num_m from t_stop where type=? and num_m=?";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, type); 
			pstmt.setInt(2, num_m); 
			rs = pstmt.executeQuery();
			if (rs.next()) {
				return rs.getInt(1);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return -1;
	}
	
	
}
