#ifndef PARTICLESSTRUCTURE_H
#define PARTICLESSTRUCTURE_H

#include <vector>
#include <boost/property_tree/ptree_fwd.hpp>

class ParticlesStructure
{
public:
    ParticlesStructure();
    ~ParticlesStructure();
    ParticlesStructure(const boost::property_tree::ptree& input);
    size_t addParticle(float xpos, float ypos, float xvel, float yvel,
    		float depth, float smoothlen, float area, short flag);


    void resizeArrays(size_t newSize);

    std::vector<float>& getAreaArray() {
		return areaArray;
	}

	std::vector<short>& getFlagArray() {
		return flagArray;
	}

	std::vector<float>& getSmoothlenArray() {
		return smoothlenArray;
	}

	std::vector<float>& getSurfElevationArray() {
		return depthArray;
	}

	std::vector<float>& getXvelArray() {
		return xvelArray;
	}

	std::vector<float>& getYvelArray() {
		return yvelArray;
	}

	std::vector<float>& getXposArray()
	{
	return xposArray;
	}
    std::vector<float>& getYposArray()
	{
	return yposArray;
	}

	size_t getNumberOfElts() const {
		return numberOfElts;
	}

private:
    size_t numberOfElts;
    std::vector<float> xposArray;
    std::vector<float> yposArray;
    std::vector<float> xvelArray;
    std::vector<float> yvelArray;
    std::vector<float> depthArray;
    std::vector<float> areaArray;
    std::vector<float> smoothlenArray;
    std::vector<short> flagArray;

};

#endif // PARTICLESSTRUCTURE_H
