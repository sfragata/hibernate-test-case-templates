package org.hibernate.bugs.entity;

import java.util.HashSet;
import java.util.Set;

public final class State
    implements java.io.Serializable {

    private static final long serialVersionUID = 7365468112552908217L;

    private Integer stateNumber;

    private String stateName;

    private Set<City> cities;

    public State(final Integer stateNumber, final String stateName) {
        super();
        this.stateNumber = stateNumber;
        this.stateName = stateName;
        this.cities = new HashSet<>();
    }

    public State() {
        super();
    }

    public Integer getStateNumber() {

        return this.stateNumber;
    }

    public void setStateNumber(
        final Integer stateNumber) {

        this.stateNumber = stateNumber;
    }

    public String getStateName() {

        return this.stateName;
    }

    public void setStateName(
        final String stateName) {

        this.stateName = stateName;
    }

    public Set<City> getCities() {

        return this.cities;
    }

    protected void setCities(
        final Set<City> cities) {

        this.cities = cities;
    }

    @Override
    public int hashCode() {

        final int prime = 31;
        int result = 1;
        result = prime * result + (this.stateNumber == null ? 0 : this.stateNumber.hashCode());
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
        final State other = (State) obj;
        if (this.stateNumber == null) {
            if (other.stateNumber != null) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {

        return String.format("State [stateNumber=%s, stateName=%s, cities=%s]", this.stateNumber, this.stateName,
            this.cities);
    }

}
