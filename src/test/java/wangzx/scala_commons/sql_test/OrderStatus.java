package wangzx.scala_commons.sql_test;

import wangzx.scala_commons.sql.IntEnum;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Created by wangzx on 2016/11/30.
 */
public enum OrderStatus implements IntEnum<OrderStatus> {

    STATUS_1( 1 ),
    STATUS_2( 2 );

    private int code;

    OrderStatus(int code) {
        this.code  = code;
    }

    public int code() { return code; }

    public static OrderStatus apply(int code) {
        return STATUS_1.fromCode( code );
    }
}
