import java.util.List; 
import java.util.Map; 
import java.util.Set; 
import java.util.ArrayList;
import java.util.HashMap; 

public class MultiMap<K,V> extends HashMap<K,List<V> > implements Map<K,List<V> > {

    public void putOneValue(K aKey, V aValue) {
	List<V> rlst;
	if (containsKey(aKey))
	    rlst = get(aKey);
	else
	    rlst = new ArrayList<V>();
	rlst.add(aValue);
	put(aKey, rlst); 
    }

    public boolean containsOneValue(V aValue) {
	for (Map.Entry<K,List<V> > e : entrySet()) {
	    List<V> rlst = e.getValue();
	    if (rlst.contains(aValue))
		return true; 
	}
	return false; 
    }

}
