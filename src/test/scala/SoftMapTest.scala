import java.lang.ref.WeakReference

import wangzx.scala_commons.sql.SoftMap

/**
 * Created by wangzx on 15/6/18.
 */
object SoftMapTest {

  def main(args: Array[String]) {

    val smap = new SoftMap[String, String]()

    val wref = new WeakReference[String]("Hello World")

    println(smap)

    for(i <- 1 to 1000*1000*10) {
      smap += (i.toString -> ("hello world " + i))
      if( i % 10000 == 0){
        println(s"i = $i size = ${smap.size}")
      }
    }

    println("before GC:size " + smap.size)
    System.gc()
    Thread.sleep(2000)

    println("after GC: size " + smap.size)

    println("wref = " + wref.get())

  }

}
