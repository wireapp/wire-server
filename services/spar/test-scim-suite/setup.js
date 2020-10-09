/* This script runs before every request in Postman / Newman */

if (pm.environment.get("wire_setup_complete") !== "complete") {
    const randomString = function() { return Math.random().toString(36).substr(2, 5); };
    const randomEmail = function () { return randomString() +  "@example.com";};

    /* Folder: User tests */
    const user1userName = randomString();
    pm.environment.set("user1userName", user1userName);
    /* NOTE: Making assumption here that externalId is an email */
    pm.environment.set("user1externalId", user1userName + "@example.com");
    const user2userName = randomString();
    pm.environment.set("user2userName", user2userName);
    pm.environment.set("user2externalId", user2userName + "@example.com");
    const user2userName2 = randomString();
    pm.environment.set("user2userName2", user2userName2);
    pm.environment.set("user2externalId2", user2userName2 + "@example.com");

    /* Folder: User tests with garbage */
    pm.environment.set("garbage1_externalId", randomEmail());
    pm.environment.set("garbage1_externalId2", randomEmail());
    pm.environment.set("garbage1_postemp2_externalId", randomEmail());
    pm.environment.set("garbage1_postemp2_userName", randomString());
    pm.environment.set("garbage1_postemp3_externalId", randomEmail());
    pm.environment.set("garbage1_postemp3_username", randomString());
    pm.environment.set("garbage1_postmisspelled_userName", randomString());
    pm.environment.set("garbage1_postmisspelled_externalId", randomEmail());
    pm.environment.set("garbage1_postenterprise_userName", randomString());
    pm.environment.set("garbage1_postenterprise_externalId", randomEmail());
    pm.environment.set("garbage1_putomalley_externalId", randomEmail());
    pm.environment.set("garbage1_putomalley_userName", randomString());

    pm.environment.set("wire_setup_complete", "complete");
}
