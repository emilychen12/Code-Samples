# Assignment for my Introduction to Computing Using Python course
# 11/16/2016
"""Classes to perform KMeans Clustering"""

import math
import random
import numpy

# HELPER FUNCTIONS FOR ASSERTS GO HERE
def is_point(thelist):
    """Return: True if thelist is a list of int or float"""
    if (type(thelist) != list):
        return False

    # All float
    okay = True
    for x in thelist:
        if (not type(x) in [int,float]):
            okay = False

    return okay


def valid_contents(dimension,contents):
    """Return: True if contents is None, or is a 2D list that is nonempty
    and the number of columns of contents is equal to dim.
    """
    if(contents is None):
        return True

    if(type(contents)!=list):
        return False
    if(len(contents)<1):
        return False

    okay=False
    for x in contents:
        if(len(x)==dimension and type(x)==list):
            okay=True

    return okay


def valid_seed_inds(dataset,k,seed_inds):
    """Return: True if seed_inds is None, or is a list of k valid indices
    into ds.
    """
    if(seed_inds is None):
        return True

    if(type(seed_inds)!=list):
        return False
    if(len(seed_inds)!=k):
        return False

    okay=False
    for x in seed_inds:
        if(dataset.getContents()[x] in dataset.getContents()):
            okay=True

    return okay


# CLASSES
class Dataset(object):
    """Instance is a dataset for k-means clustering.

    The data is stored as a list of list of numbers
    (ints or floats).  Each component list is a data point.

    Instance Attributes:
        _dimension: the point dimension for this dataset
            [int > 0. Value never changes after initialization]

        _contents: the dataset contents
            [a 2D list of numbers (float or int), possibly empty]:

    ADDITIONAL INVARIANT:
        The number of columns in _contents is equal to _dimension.
        That is, for every item _contents[i] in the list _contents,
        len(_contents[i]) == dimension.

    None of the attributes should be accessed directly outside of the class
    Dataset (e.g. in the methods of class Cluster or KMeans). Instead, this
    class has getter and setter style methods (with the appropriate
    preconditions) for modifying these values.
    """

    def __init__(self, dim, contents=None):
        """Initializer: Creates a dataset for the given point dimension.

        Note that contents (which is the initial value for
        attribute _contents) is optional. When assigning contents to the
        attribute _contents theinitializer COPIES the  list contents. If
        contents is None, the initializer assigns _contents an empty list.

        Parameter dim: the initial value for attribute _dimension.
        Precondition: dim is an int > 0.

        Parameter contents: the initial value for attribute _contents
        (optional). Precondition: contents is either None or is a 2D list
        of numbers (int or float). If contents is not None, then contents
        if not empty and the number of columns of contents is equal to dim.
        """
        assert type(dim)==int and dim > 0
        assert valid_contents(dim,contents)

        self._dim=dim
        copy=[]
        if(contents is not None):
                for x in contents:
                    copy.append(x)
        self._contents = copy


    def getDimension(self):
        """Returns: The point dimension of this data set.
        """
        return self._dim


    def getSize(self):
        """Returns: the number of elements in this data set.
        """
        return len(self._contents)


    def getContents(self):
        """Returns: The contents of this data set as a list.

        This method returns the attribute _contents directly.  Any changes
        made to this list will modify the data set.  If you want to access
        the data set, but want to protect yourself from modifying the data,
        use getPoint() instead.
        """
        copy = []
        array = self._contents
        for r in range(len(array)):
            copy.append(array[r][:])
        return copy


    def getPoint(self, i):
        """Returns: A COPY of the point at index i in this data set.

        Often, we want to access a point in the data set, but we want a copy
        to make sure that we do not accidentally modify the data set.  That
        is the purpose of this method.

        If you actually want to modify the data set, use the method
        getContents().
        That returns the list storing the data set, and any changes to that
        list will alter the data set.

        Parameter i: the index position of the point
        Precondition: i is an int that refers to a valid position in
        0..getSize()-1
        """
        assert type(i)==int and 0<=i<self.getSize()

        array = self.getContents()
        point = array[i]
        return point


    def addPoint(self,point):
        """Adds a COPY of point at the end of _contents.

        This method does not add the point directly. It adds a copy of the
        point.

        Parameter point: the point to add
        Precondition: point is a list of numbers (int or float),
        len(point) = _dimension.
        """
        assert is_point(point)
        assert len(point)==self._dim

        p=list(point)
        self._contents.append(p)


    # PROVIDED METHODS: Do not modify!
    def __str__(self):
        """Returns: String representation of the centroid of this cluster."""
        return str(self._contents)


    def __repr__(self):
        """Returns: Unambiguous representation of this cluster. """
        return str(self.__class__) + str(self)


class Cluster(object):
    """An instance is a cluster, a subset of the points in a dataset.

    A cluster is represented as a list of integers that give the indices
    in the dataset of the points contained in the cluster.  For instance,
    a cluster consisting of the points with indices 0, 4, and 5 in the
    dataset's data array would be represented by the index list [0,4,5].

    A cluster instance also contains a centroid that is used as part of
    the k-means algorithm.  This centroid is an n-D point (where n is
    the dimension of the dataset), represented as a list of n numbers,
    not as an index into the dataset.  (This is because the centroid
    is generally not a point in the dataset, but rather is usually in between
    the data points.)

    Instance Attributes:
        _dataset: the dataset this cluster is a subset of  [Dataset]

        _indices: the indices of this cluster's points in the dataset
        [list of int]

        _centroid: the centroid of this cluster  [list of numbers]

    ADDITIONAL INVARIANTS:
        len(_centroid) == _dataset.getDimension()
        0 <= _indices[i] < _dataset.getSize(), for all 0 <= i < len(_indices)
    """

    # Part A
    def __init__(self, ds, centroid):
        """Initializer: Creates a new empty cluster with the given centroid.

        Remember that a centroid is a point and hence a list.  The
        initializer COPIES the centroid; it does not use the original list.

        IMPORTANT: READ THE PRECONDITION OF ds VERY CAREFULLY

        Parameter ds: the Dataset for this cluster
        Precondition: ds is a instance of Dataset OR a subclass of Dataset

        Parameter centroid: the centroid point (which might not be a point in
        ds)
        Precondition: centroid is a list of numbers (int or float),
          len(centroid) = ds.getDimension()
        """
        assert isinstance(ds,Dataset) or issubclass(ds,Dataset)
        assert is_point(centroid)
        assert len(centroid)==ds.getDimension()

        copy = []
        for x in centroid:
            copy.append(x)
        self._ds = ds
        self._centroid=copy
        self._indices=[]


    def getCentroid(self):
        """Returns: the centroid of this cluster.

        This getter method is to protect access to the centroid.
        """
        copy=list(self._centroid)
        return copy


    def getIndices(self):
        """Returns: the indices of points in this cluster

        This method returns the attribute _indices directly.  Any changes
        made to this list will modify the cluster.
        """
        return self._indices


    def addIndex(self, index):
        """Adds the given dataset index to this cluster.

        If the index is already in this cluster, then this method leaves the
        cluster unchanged.

        Parameter index: the index of the point to add
        Precondition: index is a valid index into this cluster's dataset.
        That is, index is an int in the range 0.._dataset.getSize()-1.
        """
        assert self._ds.getContents()[index] in self._ds.getContents()
        assert type(index)==int and 0<=index<self._ds.getSize()

        if (index not in self._indices):
            self._indices.append(index)


    def clear(self):
        """Removes all points from this cluster, but leave the centroid
        unchanged.
        """
        indices=self.getIndices()
        del indices[:]


    def getContents(self):
        """Returns: a new list containing copies of the points in this
        cluster.

        The result is a list of list of numbers.  It has to be computed from
        the indices.
        """
        array=[]
        ds=self._ds
        contents=ds.getContents()
        indices=self.getIndices()
        p=list(indices)
        for x in p:
            array.append(contents[x])
        return array


    # Part B
    def distance(self, point):
        """Returns: The euclidean distance from point to this cluster's
        centroid.

        Parameter point: the point to compare to this cluster's centroid
        Precondition: point is a list of numbers (int or float),
          len(point) = _ds.getDimension()
        """
        assert is_point(point)
        assert len(point)==self._ds.getDimension()

        total=0
        dist=0
        centroid=self.getCentroid()
        for x in range(len(centroid)):
            pt1=centroid[x]
            pt2=point[x]
            total = total + float((pt1-pt2))**2.0
        dist=float(math.sqrt(total))
        return dist


    def updateCentroid(self):
        """Returns: Trues if the centroid remains unchanged; False otherwise.

        This method recomputes the _centroid attribute of this cluster. The
        new _centroid attribute is the average of the points of _contents
        (To average a point, average each coordinate separately).

        Whether the centroid "remained the same" after recomputation is
        determined by the function numpy.allclose().  The return value
        should be interpreted as an indication of whether the starting
        centroid was a "stable" position or not.

        If there are no points in the cluster, the centroid. does not change.
        """
        centroid=self.getCentroid()
        cluster=self.getContents()
        ret = True
        if (len(cluster))>0:
            coord = [float(sum(col))/len(col) for col in zip(*cluster)]
            ret=numpy.allclose(coord,centroid)
            self._centroid=list(coord)
        return ret


    # PROVIDED METHODS: Do not modify!
    def __str__(self):
        """Returns: String representation of the centroid of this cluster."""
        return str(self._centroid)


    def __repr__(self):
        """Returns: Unambiguous representation of this cluster. """
        return str(self.__class__) + str(self)


class ClusterGroup(object):
    """An instance is a set of clusters of the points in a dataset.

    Instance Attributes:
        _dataset: the dataset which this is a clustering of
            [Dataset]

        _clusters: the clusters in this clustering (not empty)
            [list of Cluster]
    """

    # Part A
    def __init__(self, ds, k, seed_inds=None):
        """Initializer: Creates a clustering of the dataset ds into k
        clusters.

        The clusters are initialized by randomly selecting k different points
        from the database to be the centroids of the clusters.  If seed_inds
        is supplied, it is a list of indices into the dataset that specifies
        which points should be the initial cluster centroids.

        IMPORTANT: READ THE PRECONDITION OF ds VERY CAREFULLY

        Parameter ds: the Dataset for this cluster group
        Precondition: ds is a instance of Dataset OR a subclass of Dataset

        Parameter k: The number of clusters (the k in k-means)
        Precondition: k is an int, 0 < k <= ds.getSize()

        Parameter seed_inds: the INDEXES of the points to start with
        Precondition: seed_inds is None, or a list of k valid indices into ds.
        """
        assert isinstance(ds,Dataset) or issubclass(ds,Dataset)
        assert type(k)==int and 0<k<=ds.getSize()
        assert valid_seed_inds(ds,k,seed_inds)

        centroids=[]
        clusters=[]
        if(seed_inds is None):
            rand_cent=random.sample(ds.getContents(),k)
            for x in rand_cent:
                centroids.append(x)
        else:
            for x in seed_inds:
                centroids.append(ds.getContents()[x])

        for x in range(k):
            clusters.append(Cluster(ds,centroids[x]))

        self._ds=ds
        self._k=k
        self._seed_inds=seed_inds
        self._clusters=clusters


    def getClusters(self):
        """Returns: The list of clusters in this object.

        This method returns the attribute _clusters directly.  Any changes
        made to this list will modify the set of clusters.
        """
        return self._clusters


    # Part B
    def _nearest_cluster(self, point):
        """Returns: Cluster nearest to point

        This method uses the distance method of each Cluster to compute the
        distance between point and the cluster centroid. It returns the
        Cluster that is the closest.

        Ties are broken in favor of clusters occurring earlier in the list of
        self._clusters.

        Parameter point: the point to match
        Precondition: point is a list of numbers (int or float),
          len(point) = self._dataset.getDimension().
        """
        assert is_point(point)
        assert len(point)==self._ds.getDimension()

        clusters=self.getClusters()
        lowest=clusters[0].distance(point)
        clust=clusters[0]
        for x in range(0,len(clusters)-1):
            distance=clusters[x+1].distance(point)
            if(distance<lowest):
                lowest = distance
                clust = clusters[x+1]
        return clust


    def _partition(self):
        """Repartitions the dataset so each point is in exactly one Cluster.
        """
        # First, clear each cluster of its points.  Then, for each point in the
        # dataset, find the nearest cluster and add the point to that cluster.
        clusters=self.getClusters()
        for x in clusters:
            x.clear()

        ds=self._ds
        contents=ds.getContents()
        for i in range(ds.getSize()):
            nearest=self._nearest_cluster(contents[i])
            nearest.addIndex(i)


    # Part C
    def _update(self):
        """Returns:True if all centroids are unchanged after an update; False
        otherwise.

        This method first updates the centroids of all clusters'.  When it is
        done, it checks whether any of them have changed. It then returns the
        appropriate value.
        """
        bools=[]
        clusters=self.getClusters()
        for x in clusters:
            bools.append(x.updateCentroid())
        if(False in bools):
            return False
        else:
            return True


    def step(self):
        """Returns: True if the algorithm converges after one step; False
        otherwise.

        This method performs one cycle of the k-means algorithm. It then
        checks if the algorithm has converged and returns True or False
        """
        # In a cycle, we partition the points and then update the means.
        self._partition()
        return self._update()


    # Part D
    def run(self, maxstep):
        """Continues clustering until either it converges or reaches maxstep
        steps.

        The stopping condition (convergence, maxsteps) is whichever comes
        first.

        Precondition maxstep: Maximum number of steps before giving up
        Precondition: maxstep is int >= 0.
        """
        # Call step repeatedly, up to maxstep times, until the algorithm
        # converges.  Stop after maxstep iterations even if the algorithm has not
        # converged.
        assert type(maxstep)==int and maxstep >=0

        x = 0
        while x < maxstep and self.step()==False:
            self.step()
            x=x+1


    # PROVIDED METHODS: Do not modify!
    def __str__(self):
        """Returns: String representation of the centroid of this cluster."""
        return str(self._clusters)


    def __repr__(self):
        """Returns: Unambiguous representation of this cluster. """
        return str(self.__class__) + str(self)
