package com.trulisoft.adagio;

import javax.swing.*;
import javax.swing.table.*;

public class JUtils {
   /**
    * Scrolls to last row visible in a table
    * 
    * @param t The table to scroll
    */
   static void scrollToLast (JTable t) {
      t.addNotify ();
      t.scrollRectToVisible (
         t.getCellRect (t.getRowCount () - 1, 0, true));
   }

   /**
    * Deletes all rows in a model
    *
    * @param t The model to empty
    */
   static void clear (DefaultTableModel t) {
      while (t.getRowCount () > 0)
         t.removeRow (0);
   }
}
