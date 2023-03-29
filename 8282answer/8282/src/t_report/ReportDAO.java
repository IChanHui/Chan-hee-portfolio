package t_report;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;

import t_answer.Answer;

public class ReportDAO {
	private Connection conn;
	private ResultSet rs;
	
	public ReportDAO() {
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
	
	public int getNext() {
		String SQL = "SELECT num_r from t_report order by num_r desc";
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
	
	public void report(String id, int num_q, int report_class, int num_attacker, int keyword){
		String SQL = "insert into t_report values (?,?,?,?,?,?)";
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			
			pstmt.setInt(1, getNext());
			pstmt.setInt(2, num_q);
			pstmt.setInt(3, report_class);
			pstmt.setInt(4, num_attacker);
			pstmt.setString(5, id);
			pstmt.setInt(6, keyword);
			pstmt.executeUpdate();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}
	
	public ArrayList<Report> getList(int keyword) {
		String SQL = "select * from t_report where keyword = ? order by num_r";
		ArrayList<Report> list = new ArrayList<Report>();
		try {
			PreparedStatement pstmt = conn.prepareStatement(SQL);
			pstmt.setInt(1, keyword);
			rs = pstmt.executeQuery();
			while (rs.next()) {
				Report report = new Report();
				report.setNum_r(rs.getInt(1));
				report.setNum_q(rs.getInt(2));
				report.setReport_class(rs.getInt(3));
				report.setNum_attacker(rs.getInt(4));
				report.setId_reporter(rs.getString(5));
				report.setKeyword(rs.getInt(6));
				list.add(report);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}
}
