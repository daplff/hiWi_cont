#include "ParticlesStructure.h"
#include <boost/property_tree/ptree.hpp>

template<typename DataType>
static void extractArrayFromParentNode (const boost::property_tree::ptree& arrayParent, std::vector<DataType>& inVector)
{
	for(auto it = arrayParent.begin(); it != arrayParent.end(); it++)
	{
		inVector.push_back( (it->second).get_value <DataType> () );
	}
}


ParticlesStructure::ParticlesStructure()
{	
	
}

ParticlesStructure::ParticlesStructure(const boost::property_tree::ptree& input)
{
	extractArrayFromParentNode(input.get_child("xposArray"),xposArray);
	extractArrayFromParentNode(input.get_child("yposArray"),yposArray);

	resizeArrays(xposArray.size());

}

size_t ParticlesStructure::addParticle(float xpos, float ypos, float xvel,
		float yvel, float depth, float smoothlen, float area, short flag) {
	//use insert to be certain to get same index everywhere
	xposArray.insert(xposArray.begin() + numberOfElts, xpos);
	yposArray.insert(yposArray.begin() + numberOfElts, ypos);
	xvelArray.insert(xvelArray.begin() + numberOfElts, xvel);
	yvelArray.insert(yvelArray.begin() + numberOfElts, yvel);
	depthArray.insert(depthArray.begin() + numberOfElts, depth);
	smoothlenArray.insert(smoothlenArray.begin() + numberOfElts, smoothlen);
	areaArray.insert(areaArray.begin() + numberOfElts, area);
	flagArray.insert(flagArray.begin() + numberOfElts, flag);

	return numberOfElts++; //return index of created element; arrays are now one bigger.
}

void ParticlesStructure::resizeArrays(size_t newSize)
{
	xposArray.resize(newSize);
	yposArray.resize(newSize);
	xvelArray.resize(newSize);
	yvelArray.resize(newSize);
	depthArray.resize(newSize);
	areaArray.resize(newSize);
	smoothlenArray.resize(newSize);
	flagArray.resize(newSize);
	numberOfElts = newSize;
}

ParticlesStructure::~ParticlesStructure()
{
}

