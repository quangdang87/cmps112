// $Id: qtest.java,v 1.1 2019-03-04 22:34:03-08 - - $

import static java.lang.System.*;

class qtest {

   public static void main (String[] args) {
      queue<String> queue = new queue<String> ();
      for (String arg : args) queue.insert (arg);
      while (! queue.empty ()) out.println (queue.remove ());
   };

};

