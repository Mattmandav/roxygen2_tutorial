#include <iostream>
#include <vector>

#include <Rcpp.h>
using namespace Rcpp;

// Point structure
struct point
{
  double x {0.0};
  double y {0.0};
};

// Checks if a point is already in a vector of points
// returns true if yes, false if no
bool repeatcheck(std::vector<point> PointVector, point p)
{
  bool repeatpoint = false;
  for(auto t : PointVector){
    if(t.x == p.x && t.y == p.y && PointVector.size()>0){
      repeatpoint = true; break;
    }
    else{
      Rcpp::checkUserInterrupt(); // User interrupt 
      repeatpoint = false;
    }
  }
  return repeatpoint;
}

// Converts a "list of vectors" to a "vector of points"
std::vector<point> lov2vop(std::list<std::vector<double>> PointListVector)
{
  std::vector<point> PointVector;
  for(std::vector<double>& xy_pair : PointListVector)
    {
      point temp_point;
      temp_point.x = xy_pair[0];
      temp_point.y = xy_pair[1];
      if(repeatcheck(PointVector, temp_point) !=  true){
        PointVector.push_back(temp_point);
      }
    }
  return PointVector;   
}

// Converts a "vector of points" to a "list of vectors"
std::list<std::vector<double>> vop2lov(std::vector<point> PointVector)
{
  std::list<std::vector<double>> PointListVector;
  for(auto p : PointVector)
     {
       std::vector<double> temp_point {p.x,p.y};
       PointListVector.push_back(temp_point);
  }
  return PointListVector;
}

// Clockwise or Anticlockwise function
// Seen in https://algs4.cs.princeton.edu/91primitives/ but redone for c++
// 0 if colinear
// 1 if clockwise
// -1 if Anticlockise
int ccw(point p, point q, point r)
{
  double doublearea;
  doublearea = (q.x-p.x)*(r.y-p.y)-(r.x-p.x)*(q.y-p.y);
  if(doublearea == 0){return 0;}
  else if(doublearea<0){return 1;}
  else{return -1;}
} 

std::list<std::vector<double>> jarvisMarch(std::list<std::vector<double>> PointListVector)
  {
     // Reformatting the data
     std::vector<point> PointVector = lov2vop(PointListVector);

     // Declare output
     std::vector<point> convexhull;
     std::list<std::vector<double>> convexhullfinal;

     // Degeneracy checks
     // Need at at least 1 point
     int NumberOfPoints = PointVector.size();
     if(NumberOfPoints == 0){
       std::cout << "WARNING: Potential missing/incorrect file path or zero points in file" << std::endl; return convexhullfinal;
     }
     
     // "You have already given me the extreme points of the convex hull for this set"
     if(NumberOfPoints == 1){
       std::cout << "WARNING: Convex hull is only 1 point" << std::endl; return vop2lov(PointVector);
     }
     if(NumberOfPoints == 2){
       std::cout << "WARNING: Convex hull has only 2 extreme points" << std::endl; return vop2lov(PointVector);
     }

     // Need at least 1 point not collinear
     bool collinear = true; int i = 0;
     while(i < (NumberOfPoints-2)){
       if(ccw(PointVector[i],PointVector[i+1],PointVector[i+2]) == 1 || ccw(PointVector[i],PointVector[i+1],PointVector[i+2]) == -1){
         collinear = false;
       } 
       i++;
     }
     if(collinear == true){
       std::cout << "WARNING: All points are collinear" << std::endl;
     }

     // Finding leftmost point
     point p = *PointVector.begin();
     for(auto x : PointVector){
	Rcpp::checkUserInterrupt(); // User interrupt
	if(x.x < p.x){p = x;}
	}
     convexhull.push_back(p);

     // Main step of algorithm 
     point q;
     // pick a point, q, in the point vector
     // iterate over all other points, r
     // if r "to the left" of line PQ then set q=r
     // after we have done this then line PQ is an edge of the hull
     // add q to our convex hull (if not in already)
     // set p=q and repeat until have tried all points q
     for(auto x : PointVector)
     {
     q = x;
       for(auto r : PointVector){
         if(ccw(p,q,r) == -1){q = r;}
         Rcpp::checkUserInterrupt(); // User interrupt 
       }
       if(repeatcheck(convexhull, q) !=  true){
         convexhull.push_back(q);
       }
       p = q; 
     }


     // Reformatting the data back into list of vectors
     convexhullfinal = vop2lov(convexhull);

     // Output
     return convexhullfinal;
  }

//' ConvexHullMD
//'
//' Uses Jarvis March algorithm to find the convex hull of a list of 2D vectors.
//'
//' @param LofV List of 2D vectors.
//' @return List of 2D vectors representing vertices of the convex hull.
//[[Rcpp::export]]
std::list<std::vector<double>> ConvexHullMD(std::list<std::vector<double>> LofV)
{
  std::list<std::vector<double>> convexhull_pgram = jarvisMarch(LofV);
  return convexhull_pgram;
}
