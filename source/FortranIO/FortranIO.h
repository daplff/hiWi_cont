#ifndef FORTRANIO_H
#define FORTRANIO_H

class ParticlesStructure;

class FortranIO
{
public:
	FortranIO();
	~FortranIO();
	void setRuntimeVariables(ParticlesStructure& particles);
	void getRuntimeVariables(ParticlesStructure& particles);
	void getRuntimeParameters();

	bool isGetSurfElev() const {
		return getSurfElev;
	}

	void setGetSurfElev(bool getSurfElev) {
		this->getSurfElev = getSurfElev;
	}

	bool isSetSurfElev() const {
		return setSurfElev;
	}

	void setSetSurfElev(bool setSurfElev) {
		this->setSurfElev = setSurfElev;
	}

	void changeGetAllVars(bool input)
	{
		getXpos= input;
		getYpos= input;
		getXvel= input;
		getYvel= input;
		getArea= input;
		getSmoothlen= input;
		getFlag= input;
	}
	void changeSetAllVars(bool input)
	{
		setXpos= input;
		setYpos= input;
		setXvel= input;
		setYvel= input;
		setArea= input;
		setSmoothlen= input;
		setFlag= input;
	}

    // getters and setters
	bool isGetXpos() const {
		return getXpos;
	}

	void setGetXpos(bool getXpos) {
		this->getXpos = getXpos;
	}

	bool isSetXpos() const {
		return setXpos;
	}

	void setSetXpos(bool setXpos) {
		this->setXpos = setXpos;
	}

	bool isGetArea() const {
		return getArea;
	}

	void setGetArea(bool getArea) {
		this->getArea = getArea;
	}

	bool isGetFlag() const {
		return getFlag;
	}

	void setGetFlag(bool getFlag) {
		this->getFlag = getFlag;
	}

	bool isGetSmoothlen() const {
		return getSmoothlen;
	}

	void setGetSmoothlen(bool getSmoothlen) {
		this->getSmoothlen = getSmoothlen;
	}

	bool isGetXvel() const {
		return getXvel;
	}

	void setGetXvel(bool getXvel) {
		this->getXvel = getXvel;
	}

	bool isGetYpos() const {
		return getYpos;
	}

	void setGetYpos(bool getYpos) {
		this->getYpos = getYpos;
	}

	bool isGetYvel() const {
		return getYvel;
	}

	void setGetYvel(bool getYvel) {
		this->getYvel = getYvel;
	}

	bool isSetArea() const {
		return setArea;
	}

	void setSetArea(bool setArea) {
		this->setArea = setArea;
	}

	bool isSetFlag() const {
		return setFlag;
	}

	void setSetFlag(bool setFlag) {
		this->setFlag = setFlag;
	}

	bool isSetSmoothlen() const {
		return setSmoothlen;
	}

	void setSetSmoothlen(bool setSmoothlen) {
		this->setSmoothlen = setSmoothlen;
	}

	bool isSetXvel() const {
		return setXvel;
	}

	void setSetXvel(bool setXvel) {
		this->setXvel = setXvel;
	}

	bool isSetYpos() const {
		return setYpos;
	}

	void setSetYpos(bool setYpos) {
		this->setYpos = setYpos;
	}

	bool isSetYvel() const {
		return setYvel;
	}

	void setSetYvel(bool setYvel) {
		this->setYvel = setYvel;
	}

private:
	//flags for I/O
	bool getXpos;
	bool getYpos;
	bool getXvel;
	bool getYvel;
	bool getSurfElev;
	bool getArea;
	bool getSmoothlen;
	bool getFlag;
	bool setXpos;
	bool setYpos;
	bool setXvel;
	bool setYvel;
	bool setSurfElev;
	bool setArea;
	bool setSmoothlen;
	bool setFlag;

};

#endif // FORTRANIO_H
