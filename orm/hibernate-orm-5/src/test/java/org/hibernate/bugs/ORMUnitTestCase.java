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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.bugs.entity.City;
import org.hibernate.cfg.AvailableSettings;
import org.hibernate.cfg.Configuration;
import org.hibernate.query.NativeQuery;
import org.hibernate.testing.junit4.BaseCoreFunctionalTestCase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ORMUnitTestCase
    extends BaseCoreFunctionalTestCase {

    private static final String BASE_COUNTRY_NAME = "country";

    private static final String BASE_CITY_NAME = "city1";

    private static final String CITY_NAME = "city2";

    @Override
    protected Class[] getAnnotatedClasses() {

        return new Class[] {};
    }

    // If you use *.hbm.xml mappings, instead of annotations, add the mappings here.
    @Override
    protected String[] getMappings() {

        return new String[] { "City.hbm.xml" };
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

    @Before
    public void before() {

        final City city = new City(1, BASE_CITY_NAME, BASE_COUNTRY_NAME);
        final Session s = openSession();
        final Transaction tx = s.beginTransaction();
        s.save(city);
        tx.commit();
        s.close();
    }

    @After
    public void after() {

        final City city = new City(1, BASE_CITY_NAME, BASE_COUNTRY_NAME);
        try (final Session s = openSession()) {
            final Transaction tx = s.beginTransaction();
            s.delete(city);
            tx.commit();
        }
    }

    /**
     * When use UPDATE NAMED QUERY the comment doesn't works
     */
    @SuppressWarnings("rawtypes")
    @Test
    public void hhh11640NamedQueryUpdateDoesntLogQueryComment()
        throws Exception {

        try (final Session s = openSession()) {
            final NativeQuery namedNativeQuery = s.getNamedNativeQuery("updateCityName");
            namedNativeQuery.setParameter(0, CITY_NAME);
            namedNativeQuery.setParameter(1, 1);
            final Transaction tx = s.beginTransaction();
            final int rows = namedNativeQuery.executeUpdate();
            tx.commit();
            assertEquals(1, rows);
        }
        // Check log and you will see that this named query is logged WITHOUT the comments
    }

    /**
     * When use DELETE NAMED QUERY the comment doesn't works
     */
    @SuppressWarnings("rawtypes")
    @Test
    public void hhh11640NamedQueryDeleteDoesntLogQueryComment()
        throws Exception {

        try (final Session s = openSession()) {
            final NativeQuery namedNativeQuery = s.getNamedNativeQuery("deleteCity");
            namedNativeQuery.setParameter(0, 1);
            final Transaction tx = s.beginTransaction();
            final int rows = namedNativeQuery.executeUpdate();
            tx.commit();
            assertEquals(1, rows);
        }
        // Check log and you will see that this named query is logged WITHOUT the comments
    }

    /**
     * When use SELECT NAMED QUERY the comment works
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Test
    public void hhh11640NamedQuerySelectDoesLogQueryComment()
        throws Exception {

        try (final Session s = openSession()) {
            final NativeQuery namedNativeQuery = s.getNamedNativeQuery("findByName");
            namedNativeQuery.setParameter(0, BASE_CITY_NAME);
            final List<Object[]> resultList = namedNativeQuery.getResultList();
            assertNotNull(resultList);
            assertEquals(1, resultList.size());
        }
        // Check log and you will see that this named query is logged WITH the comments
    }

}
