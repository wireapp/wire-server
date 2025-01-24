# How to set up SSO integration with Microsoft Entra ID

## Purpose and Scope
This document is for current and perspective users of the Wire cloud, who want to manage their users with Microsoft Entra ID, in Azure.

## Preprequisites

Before you begin, we are going to assume you have the following:
- An account on <http://azure.microsoft.com>
  * admin access to that account, sufficient to add an application, and manage users.
- A team on https://app.wire.com/ or another Wire backend, and admin access to that team.

## Process

### Creating a New Application
Go to [portal.azure.com](https://portal.azure.com/), and login. You should be brought to the 'Microsoft Azure' home page.
 * In the 'Azure services' section, click on 'Microsoft Entra ID'

```{image} 00.png
```

You should now see the 'Default Directory | Overview' page.
 * In the menu to your left, 'Manage' should already be selected. under manage, click on 'Enterprise Applications'

```{image} 01.png
```

This should bring you to the 'Enterprise applications| All applications' page.
 * Click on 'New Application':

```{image} 02.png
```

This brings you to the 'Browse Microsoft Entra Gallery' page.
 * Select 'Create your own application':

```{image} 03.png
```

This should have opened a 'Create your own application' window in the current page.
 * Fill in the user-visible app name with the name for this application that you want your users to see.
 * Leave the option selected next to 'Integrate with any other application you don\'t find in the gallery', then click 'add':

```{image} 04.png
```

The app is now created, but is not yet configured. If you get lost, you can always get back to it by selecting its name from the 'Enterprise applications| All applications' page.

### Configuring your new application

If you followed the prior step, you should now be at the screen containing the settings for your application. If you didn't, please find your app by opening azure, going to 'Microsoft Entra ID', and clicking on 'Enterprise Applications' in the left hand menu.

In the 'Getting Started' section of the page containing your application definition, Click on 'get started' in the '2. Set up single sign on' box.

```{image} 05.png
```

You should now see the 'Single sign-on' page for your application definition.
 * Click on the 'SAML' box with the puzzle piece in it.

```{image} 06.png
```

The next page is the 'SAML-based Sign-on' page for your application definition. There is a helpful configuration guide at the top of the page which you can consult if you have any azure-specific questions.

Let's go straight to adding the two config parameters that are required, and saving.

In the 'Basic SAML Configuration' section, click on 'Edit'. This will bring up the 'Basic SAML Configuration' window.
 * Enter <https://prod-nginz-https.wire.com/sso/finalize-login> for both 'Identifier (Entity ID)' and 'Reply URL(Assertion Consumer Service URL)'.

```{image} 07.png
```

```{image} 08.png
```

Click on 'test later':

```{image} 09.png
```

Finally, you need to assign users to the newly created and configured application:

```{image} 11.png
```

```{image} 12.png
```

```{image} 13.png
```

```{image} 14.png
```

```{image} 15.png
```

And that's it!  You are now ready to set up your wire team for SAML SSO with the XML metadata file you downloaed above.

## Further reading

- technical concepts overview:
  : - <https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-saml-protocol-reference>
    - <https://docs.microsoft.com/en-us/azure/active-directory/develop/single-sign-on-saml-protocol>
- how to create an app:
  : - <https://docs.microsoft.com/en-us/azure/active-directory/develop/quickstart-register-app>
- how to configure SAML2.0 SSO:
  : - <https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/what-is-single-sign-on#saml-sso>
    - <https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/configure-single-sign-on-non-gallery-applications>
