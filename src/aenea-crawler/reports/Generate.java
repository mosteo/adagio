import com.trulisoft.g2.*;

public class Generate {
   public static void main (String args[]) {

      Stats stats = new Stats ();

      stats.toFile ("crawl1.png", 1.0, false);
      System.out.println ("Generated crawl1");
      stats.toFile ("crawl2.png", 7.0, false);
      System.out.println ("Generated crawl2");
      stats.toFile ("crawl3.png", 30.0, false);
      System.out.println ("Generated crawl3");
      stats.toFile ("crawl4.png", 365.0, false);
      System.out.println ("Generated crawl4");
      stats.toFile ("crawl5.png", 0.0, false);
      System.out.println ("Generated crawl5");
      
      CountGraphs g = new CountGraphs();
      
      // VENDORS
      g.toFile("vendors1.png", 1.0, "vendor", 10, false);
      System.out.println("Generated vendors1");
      g.toFile("vendors2.png", 7.0, "vendor", 10, false);
      System.out.println("Generated vendors2");
      g.toFile("vendors3.png", 30.0, "vendor", 10, false);
      System.out.println("Generated vendors3");
      g.toFile("vendors4.png", 365.0, "vendor", 10, false);
      System.out.println("Generated vendors4");
      g.toFile("vendors5.png", 0.0, "vendor", 10, false);
      System.out.println("Generated vendors5");
      
      // VERSIONS
      g.toFile("versions7.png", 7.0, "version", 10, false);
      System.out.println("Generated versions7");
      g.toFile("versions30.png", 30.0, "version", 10, false);
      System.out.println("Generated versions30");
      g.toFile("versions0.png", 0.0, "version", 10, false);
      System.out.println("Generated versions0");
      
      // COUNTRIES
      g.toFile("countries7.png", 7.0, "country", 10, false);
      System.out.println("Generated countries7");
      g.toFile("countries30.png", 30.0, "country", 10, false);
      System.out.println("Generated countries30");
      g.toFile("countries0.png", 0.0, "country", 10, false);
      System.out.println("Generated countries0");
   }
}
