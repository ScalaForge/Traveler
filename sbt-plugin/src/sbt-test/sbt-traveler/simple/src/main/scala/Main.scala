object Main {
  def main(args: Array[String]): Unit = 
    if(getClass().getClassLoader().getResource("traveler-helpers/test-thing-helper.so") == null) {
       println("bad resource") 
       sys.exit(1)
    }
}
