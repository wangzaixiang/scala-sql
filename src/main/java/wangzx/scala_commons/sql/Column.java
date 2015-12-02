package wangzx.scala_commons.sql;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.FIELD,ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Column {

	String name() default "";

	boolean isTransient() default false;

	/**
	 * when mapping a field like Option[Int], we need annotated the gereric type
	 * since all primitive type is erased to Option[Object]
	 *
	 * TODO refactor to a good name
     */
	Class<?> optionalType() default Object.class;

}
