package Charles.Prime_Numbers is

   pragma Pure;

   type Primes_Type is
      array (Positive range <>) of Positive;

   Primes : constant Primes_Type :=
     (5, 7, 11, 13, 17, 19, 23, 31, --for debugging
      53,         97,         193,       389,       769,
      1543,       3079,       6151,      12289,     24593,
      49157,      98317,      196613,    393241,    786433,
      1572869,    3145739,    6291469,   12582917,  25165843,
      50331653,   100663319,  201326611, 402653189, 805306457,
      1610612741);

   function To_Prime (Length : Integer'Base) 
      return Positive;

end Charles.Prime_Numbers;
