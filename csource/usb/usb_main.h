#ifndef __USB_MAIN
#define __USB_MAIN

#include <stdint.h>
#include "main.h"
#include "hw_config.h"
#include "usb_lib.h"
#include "usb_desc.h"
#include "usb_pwr.h"

void usb_ionic_prepare(void);
void handle_usb_loop(void);

#endif /* __USB_MAIN */
