package Adagio.Network.Tasks is

   -- Launchs automatic connecting to every configured network, 
   -- polling every Period miliseconds to reconnect if dropped
   procedure Start(Period: Duration := 1.0);
   
end Adagio.Network.Tasks;
