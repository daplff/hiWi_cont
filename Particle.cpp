#include "Particle.h"
#include <boost/property_tree/ptree.hpp>

Particle::Particle()
{
}

Particle::Particle(const boost::property_tree::ptree& input)
{
	posX = input.get<double>("posX");
	posY = input.get<double>("posY");
	velX = input.get<double>("velX");
	velY = input.get<double>("velY");
	smoothLen = input.get<double>("smoothLen");
	elevation = input.get<double>("elevation");
	
}


Particle::~Particle()
{
}

