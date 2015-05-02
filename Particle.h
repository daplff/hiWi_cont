#ifndef PARTICLE_H
#define PARTICLE_H

#include <boost/property_tree/ptree_fwd.hpp>

class Particle
{
public:
	Particle();
	Particle(const boost::property_tree::ptree& input);
	~Particle();

	void setElevation(double elevation) {this->elevation = elevation;}
	void setPosX(double posX) {this->posX = posX;}
	void setPosY(double posY) {this->posY = posY;}
	void setSmoothLen(double smoothLen) {this->smoothLen = smoothLen;}
	void setVelX(double velX) {this->velX = velX;}
	void setVelY(double velY) {this->velY = velY;}
	double getElevation() const {return elevation;}
	double getPosX() const {return posX;}
	double getPosY() const {return posY;}
	double getSmoothLen() const {return smoothLen;}
	double getVelX() const {return velX;}
	double getVelY() const {return velY;}
private:
	double posX, posY, velX, velY;
	double smoothLen;
	double elevation;
	
};

#endif // PARTICLE_H
