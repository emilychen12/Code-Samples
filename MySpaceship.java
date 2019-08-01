/* Assignment for my Object-Oriented Programming and Data Structures course. */

package student;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import models.NodeStatus;
import models.RescueStage;
import models.ReturnStage;
import models.Spaceship;
import models.Node;

/** An instance implements the methods needed to complete the mission */
public class MySpaceship extends Spaceship {

	/**
	 * Explore the galaxy, trying to find the missing spaceship that has crashed
	 * on Planet X in as little time as possible. Once you find the missing
	 * spaceship, you must return from the function in order to symbolize that
	 * you've rescued it. If you continue to move after finding the spaceship
	 * rather than returning, it will not count. If you return from this
	 * function while not on Planet X, it will count as a failure.
	 *
	 * At every step, you only know your current planet's ID and the ID of all
	 * neighboring planets, as well as the ping from the missing spaceship.
	 *
	 * In order to get information about the current state, use functions
	 * currentLocation(), neighbors(), and getPing() in RescueStage. You know
	 * you are standing on Planet X when foundSpaceship() is true.
	 *
	 * Use function moveTo(long id) in RescueStage to move to a neighboring
	 * planet by its ID. Doing this will change state to reflect your new
	 * position.
	 */
	@Override
	public void rescue(RescueStage state) {
		// TODO : Find the missing spaceship
		depthFirstSearch(state,new RescuePath(),null);
	}

	/** An instance contains the path the ship needs to travel and the
	 * accumulated distance. */
    private class RescuePath {
    	private Stack<Long> paths;
    	private double dist;

    	public RescuePath() {
    		paths = new Stack<Long>();
    		dist = 0.0;
    	}
    }

    /** The ship visits all the planets that can be reached from the current
     * planet to find the shortest path. The ship returns to the previous planet
     * when no new planets can be traversed. When Planet X is found, the ship
     * follows the path founded. */
    public RescuePath depthFirstSearch(RescueStage state, RescuePath path,
    	RescuePath shortest) {
    	long origId = state.currentLocation();
    	path.paths.push(origId);
    	if (state.foundSpaceship()) return path;
    	LinkedList<NodeStatus> neighbors = new LinkedList<NodeStatus>();
    	// sorting shortens the processing complexity
    	for(NodeStatus n: state.neighbors()) {
    		neighbors.add(n);
    		Collections.sort(neighbors);
    	}
   	    for(NodeStatus n: neighbors) {
   	        long currentId = n.getId();
   	        if (!path.paths.contains(currentId)) {
   	        	double newDist = path.dist + 1/n.getPingToTarget();
   	            if (shortest == null || newDist < shortest.dist) {
   	            	state.moveTo(currentId);
   	            	path.dist = newDist;
   	           	  	RescuePath newPath=depthFirstSearch(state, path, shortest);
    	            if (newPath != null) {
    	            	shortest = newPath;
    	            }
    	            else {
    	            	state.moveTo(origId);
    	            }
    	        }
    	    }
    	}
   	    return shortest;
    }

	/**
	 * Get back to Earth, avoiding hostile troops and searching for speed
	 * upgrades on the way. Traveling through 3 or more planets that are hostile
	 * will prevent you from ever returning to Earth.
	 *Res
	 * You now have access to the entire underlying graph, which can be accessed
	 * through ScramState. currentNode() and getEarth() will return Node objects
	 * of interest, and getNodes() will return a collection of all nodes in the
	 * graph.
	 *
	 * You may use state.grabSpeedUpgrade() to get a speed upgrade if there is
	 * one, and can check whether a planet is hostile using the isHostile
	 * function in the Node class.
	 *
	 * You must return from this function while on Earth. Returning from the
	 * wrong location will be considered a failed run.
	 *
	 * You will always be able to return to Earth without passing through three
	 * hostile planets. However, returning to Earth faster will result in a
	 * better score, so you should look for ways to optimize your return.
	 */
	@Override
	public void returnToEarth(ReturnStage state) {
		// TODO: Return to Earth
		List<Node> path = Paths.shortestPath(state.currentNode(),
				state.getEarth());
		if(path != null && path.size() > 0) {
			path.remove(0);
			for(Node n: path) {
				if(state.currentNode().hasSpeedUpgrade()) {
					state.grabSpeedUpgrade();
				}
				state.moveTo(n);
			}
		}
	}
}
