/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.hibernate.bugs;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.bugs.entity.City;
import org.hibernate.bugs.entity.State;
import org.hibernate.cfg.AvailableSettings;
import org.hibernate.cfg.Configuration;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Restrictions;
import org.hibernate.testing.junit4.BaseCoreFunctionalTestCase;
import org.junit.Test;

public class ORMUnitTestCase
    extends BaseCoreFunctionalTestCase {

    private static final String CITY_NAME_1 = "city1";

    private static final String CITY_NAME_2 = "city2";

    private static final String STATE_NAME = "state1";

    @Override
    protected Class[] getAnnotatedClasses() {

        return new Class[] {};
    }

    // If you use *.hbm.xml mappings, instead of annotations, add the mappings here.
    @Override
    protected String[] getMappings() {

        return new String[] { "City.hbm.xml", "State.hbm.xml" };
    }

    @Override
    protected String getBaseForMappings() {

        return "org/hibernate/test/";
    }

    @Override
    protected void configure(
        final Configuration configuration) {

        super.configure(configuration);

        configuration.setProperty(AvailableSettings.SHOW_SQL, Boolean.TRUE.toString());
        configuration.setProperty(AvailableSettings.FORMAT_SQL, Boolean.FALSE.toString());
        configuration.setProperty(AvailableSettings.GENERATE_STATISTICS, Boolean.TRUE.toString());
        configuration.setProperty(AvailableSettings.LOG_SESSION_METRICS, Boolean.FALSE.toString());

        // Enable query comments
        configuration.setProperty(AvailableSettings.USE_SQL_COMMENTS, Boolean.TRUE.toString());

    }

    @Override
    protected void prepareTest()
        throws Exception {

        final State state = new State(1, STATE_NAME);

        final City city1 = new City(1, CITY_NAME_1);
        final City city2 = new City(2, CITY_NAME_2);

        city2.setState(state);
        city1.setState(state);
        state.getCities().add(city1);
        state.getCities().add(city2);
        try (final Session s = openSession()) {
            final Transaction tx = s.beginTransaction();
            s.save(state);
            tx.commit();
        }
    }

    @Override
    protected void cleanupTest()
        throws Exception {

        try (final Session s = openSession()) {
            final Transaction tx = s.beginTransaction();
            final State state = s.load(State.class, 1);
            s.delete(state);
            tx.commit();
        }
    }

    /**
     * If you check the sql logs the comment doesn't work for the children (cities)
     */
    @Test
    public void hhh11641ChildrenDoesntLogQueryCommentInCriteriaQuery()
        throws Exception {

        final EntityManager entityManager = sessionFactory().createEntityManager();

        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

        final CriteriaQuery<Object> criteria = criteriaBuilder.createQuery();

        final Root<State> stateRoot = criteria.from(State.class);
        criteria.select(stateRoot);
        criteria.where(criteriaBuilder.equal(stateRoot.get("stateNumber"), 1));
        final State state = (State) entityManager.createQuery(criteria).getSingleResult();

        assertNotNull(state);
        final Set<City> cities = state.getCities();
        assertNotNull(cities);
        assertFalse(cities.isEmpty());

    }

    /**
     * If you check the sql logs the comment doesn't work for the children (cities)
     */
    @Test
    public void hhh11641ChildrenDoesntLogQueryCommentInDetachedCriteria()
        throws Exception {

        try (final Session s = openSession()) {

            final DetachedCriteria criteria = DetachedCriteria.forClass(State.class);

            criteria.add(Restrictions.eq("stateNumber", 1));

            final State state = (State) criteria.getExecutableCriteria(s).uniqueResult();
            assertNotNull(state);
            final Set<City> cities = state.getCities();
            assertNotNull(cities);
            assertFalse(cities.isEmpty());
        }

    }

}
