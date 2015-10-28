package wangzx.scala_commons.sql;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Table {
	/**
	 * table name
	 */
	String value() default "";
	
	String catelog() default "";

	/**
	 * when true mapping userName to user_name
	 * otherwise mapping userName to username
	 * @return
	 */
	boolean camelToUnderscore() default false;
}
