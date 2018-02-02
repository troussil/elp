package helper; 

class MissingOptionValue extends Exception {

    private String optionName; 

    public MissingOptionValue(String optionName) {
	this.optionName = optionName; 
    } 

    @Override
    public String getMessage() {
	return "Value of " + optionName + " is missing"; 
    }
}
