<?php

/**
 * This class concretely implements DaGdController for the sole purpose of being
 * used by CLI applications/scripts which need access to a controller instance.
 *
 * The primary usecase is to call into resources which want to pull a database
 * handler from the controller instance. This gives them a controller instance
 * from which to do that.
 *
 * Usage looks like this:
 *
 *   $ctrl = new DaGdCLIFakeController();
 *   $ctrl->setWriteDB($dbh)->setReadDB($dbh);
 *
 * $ctrl can then be used in places which expect a controller to have handlers
 * ready to go.
 *
 * NOTE: There is currently no (fake) request associated with the controller.
 * This effectively limits its utility to the usecase given above.
 */
final class DaGdCLIFakeController extends DaGdController {
}
