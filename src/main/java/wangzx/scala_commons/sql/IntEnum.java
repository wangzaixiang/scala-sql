package wangzx.scala_commons.sql;

/**
 * Created by wangzx on 2016/11/30.
 */
public interface  IntEnum<T> {

    int code();

    default T fromCode(int code) {
        return null;
    }

}
