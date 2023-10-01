import { Transition } from "@headlessui/react";
import { Fragment } from "react";

const MenuTransition: React.FC<{ children: React.ReactNode }> = ({
  children,
}) => (
  <Transition
    enter="transition"
    enterFrom="-translate-y-2 opacity-0"
    enterTo="translate-y-0 opacity-100"
    leave="transition"
    leaveFrom="translate-y-0 opacity-100"
    leaveTo="-translate-y-2 opacity-0"
    as={Fragment}
  >
    {children}
  </Transition>
);

export default MenuTransition;
