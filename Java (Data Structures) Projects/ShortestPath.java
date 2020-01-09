/* Assignment for my Object-Oriented Programming and Data Structures course. */

package student;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import graph.Edge;
import graph.Node;

/** This class contains Dijkstra's shortest-path algorithm and some other
 *  methods. */
public class Paths {

    /** Return the shortest path from start to end, or the empty list if a path
     * does not exist.
     * Note: The empty list is NOT "null"; it is a list with 0 elements. */
    public static List<Node> shortestPath(Node start, Node end) {
    	/* TODO Read note A7 FAQs on the course piazza for ALL details. */
    	/* HashMap data contains a data entry (backpointer, distance) for
    	 * each node of the Frontier set and the Settled set */
    	HashMap<Node,SFdata> data = new HashMap<Node,SFdata>();
    	/* As described in the abstract version of the algorithm in the lecture
    	 * notes, F contains the neighbor node(s) w of node f */
    	Heap<Node> F = new Heap<Node>();
    	data.put(start,new SFdata(0,null));
    	F.add(start,0);
    	while(F.size() != 0) {
    		Node f = F.poll();
    		/* Theorem: For a node f in F with minimum d value, d[f] is its
    		 * shortest path length */
    		if(f.equals(end)) return constructPath(end,data);
    		for(Edge e: f.getExits()) {
    			Node w = e.getOther(f);
				int dist = e.length+data.get(f).distance;
    			if(!data.containsKey(w)) {
    				F.add(w,dist);
    				data.put(w,new SFdata(dist,f));
    			}
    			else if(dist < data.get(w).distance) {
    				F.updatePriority(w,dist);
    				data.put(w, new SFdata(dist,f));
    			}
    		}
    	}
    	if(!data.containsKey(end)) return new LinkedList<Node>();
        return constructPath(end,data);
    }

    /** Return the path from the start node to node end.
     *  Precondition: nData contains all the necessary information about
     *  the path. */
    public static List<Node> constructPath(Node end, HashMap<Node,SFdata> nData)
    {
        LinkedList<Node> path= new LinkedList<Node>();
        Node p= end;
        // invariant: All the nodes from p's successor to the end are in
        //            path, in reverse order.
        while (p != null) {
            path.addFirst(p);
            p= nData.get(p).backPointer;
        }
        return path;
    }

    /** Return the sum of the weights of the edges on path path. */
    public static int pathDistance(List<Node> path) {
        if (path.size() == 0) return 0;
        synchronized(path) {
            Iterator<Node> iter= path.iterator();
            Node p= iter.next();  // First node on path
            int s= 0;
            // invariant: s = sum of weights of edges from start to p
            while (iter.hasNext()) {
                Node q= iter.next();
                s= s + p.getEdge(q).length;
                p= q;
            }
            return s;
        }
    }

    /** An instance contains information about a node: the previous node
     *  on a shortest path from the start node to this node and the distance
     *  of this node from the start node. */
    private static class SFdata {
        private Node backPointer; // backpointer on path from start node to this
        						  // one
        private int distance; // distance from start node to this one

        /** Constructor: an instance with distance d from the start node and
         *  backpointer p.*/
        private SFdata(int d, Node p) {
            distance= d;     // Distance from start node to this one.
            backPointer= p;  // Backpointer on the path (null if start node)
        }

        /** return a representation of this instance. */
        public String toString() {
            return "dist " + distance + ", bckptr " + backPointer;
        }
    }
}
