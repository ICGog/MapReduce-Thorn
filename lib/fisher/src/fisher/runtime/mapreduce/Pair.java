package fisher.runtime.mapreduce;
import java.util.*;

public class Pair {
        byte[] key;
        byte[] value;
    
        Pair(byte[] key, byte[] value) {
            this.key = key;
            this.value = value;
        }
        
        int size() {
            return key.length + value.length;
        }
    }
