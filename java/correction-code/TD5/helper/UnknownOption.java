package helper; 

class UnknownOption extends Exception {

    private String optionName; 

    public UnknownOption(String optionName) {
	this.optionName = optionName; 
    } 

    @Override
    public String getMessage() {
	return optionName + " is not a possible option"; 
    }
}
