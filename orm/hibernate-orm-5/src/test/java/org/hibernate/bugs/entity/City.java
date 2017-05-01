package org.hibernate.bugs.entity;

public final class City
    implements java.io.Serializable {

    private static final long serialVersionUID = -2696788155112918826L;

    private Integer cityNumber;

    private String cityName;

    private State state;

    public City(final Integer cityNumber, final String cityName) {
        super();
        this.cityNumber = cityNumber;
        this.cityName = cityName;
    }

    public City() {
        super();
    }

    public Integer getCityNumber() {

        return this.cityNumber;
    }

    public void setCityNumber(
        final Integer cityNumber) {

        this.cityNumber = cityNumber;
    }

    public String getCityName() {

        return this.cityName;
    }

    public void setCityName(
        final String cityName) {

        this.cityName = cityName;
    }

    public State getState() {

        return this.state;
    }

    public void setState(
        final State state) {

        this.state = state;
    }

    @Override
    public int hashCode() {

        final int prime = 31;
        int result = 1;
        result = prime * result + (this.cityNumber == null ? 0 : this.cityNumber.hashCode());
        return result;
    }

    @Override
    public boolean equals(
        final Object obj) {

        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final City other = (City) obj;
        if (this.cityNumber == null) {
            if (other.cityNumber != null) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {

        return String.format("City [cityNumber=%s, cityName=%s]", this.cityNumber, this.cityName);
    }

}
